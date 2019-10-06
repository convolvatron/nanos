#include <unix_internal.h>
#include <page.h>

#define PF_DEBUG
#ifdef PF_DEBUG
#define pf_debug(x, ...) thread_log(current, x, ##__VA_ARGS__);
#else
#define pf_debug(x, ...)
#endif

static boolean vmap_attr_equal(vmap a, vmap b)
{
    return a->flags == b->flags;
}

static inline u64 page_map_flags(u64 vmflags)
{
    u64 flags = PAGE_NO_FAT | PAGE_USER;
    if ((vmflags & VMAP_FLAG_EXEC) == 0)
        flags |= PAGE_NO_EXEC;
    if ((vmflags & VMAP_FLAG_WRITABLE))
        flags |= PAGE_WRITABLE;
    return flags;
}

static void
deliver_segv(u64 vaddr, s32 si_code)
{
    struct siginfo s = {
        .si_signo = SIGSEGV,
         /* man sigaction: "si_errno is generally unused on Linux" */
        .si_errno = 0,
        .si_code = si_code,
        .sifields.sigfault = {
            .addr = vaddr,
        }
    };

    pf_debug("delivering SIGSEGV; vaddr 0x%lx si_code %s",
        vaddr, (si_code == SEGV_MAPERR) ? "SEGV_MAPPER" : "SEGV_ACCERR"
    );

    deliver_signal_to_thread(current, &s); 
    thread_yield();
}

static boolean do_demand_page(vmap vm, u64 vaddr)
{
    u32 flags = VMAP_FLAG_MMAP | VMAP_FLAG_ANONYMOUS;
    if ((vm->flags & flags) != flags) {
        msg_err("vaddr 0x%lx matched vmap with invalid flags (0x%x)\n",
                vaddr, vm->flags);
        return false;
    }

    /* XXX make free list */
    kernel_heaps kh = get_kernel_heaps();
    u64 paddr = allocate_u64(heap_physical(kh), PAGESIZE);
    if (paddr == INVALID_PHYSICAL) {
        msg_err("cannot get physical page; OOM\n");
        return false;
    }

    u64 vaddr_aligned = vaddr & ~MASK(PAGELOG);
    map(vaddr_aligned, paddr, PAGESIZE, page_map_flags(vm->flags), heap_pages(kh));
    zero(pointer_from_u64(vaddr_aligned), PAGESIZE);

    return true;
}

boolean unix_fault_page(u64 vaddr, context frame)
{
    process p = current->p;
    u64 error_code = frame[FRAME_ERROR_CODE];

    vmap vm = (vmap)rangemap_lookup(p->vmaps, vaddr);

    /* no vmap --> send access violation */
    if (vm == INVALID_ADDRESS) {
        deliver_segv(vaddr, SEGV_MAPERR); /* does not return */
        assert(0);
    }

    /* vmap found, with protection violation set --> send prot violation */
    if (error_code & FRAME_ERROR_PF_P) {
        if (error_code & FRAME_ERROR_PF_RSV) {
            /* no SEGV on reserved PTEs */
            msg_err("bug: pte reserved\n");
#ifndef BOOT
            dump_ptes(pointer_from_u64(vaddr));
#endif
            return false;
        }

        pf_debug("\npage protection violation\naddr 0x%lx, rip 0x%lx, "
                "error %s%s%s vm->flags (%s%s%s%s)", 
                vaddr, frame[FRAME_RIP],
                (error_code & FRAME_ERROR_PF_RW) ? "W" : "R",
                (error_code & FRAME_ERROR_PF_US) ? "U" : "S",
                (error_code & FRAME_ERROR_PF_ID) ? "I" : "D",
                (vm->flags & VMAP_FLAG_MMAP) ? "mmap " : "",
                (vm->flags & VMAP_FLAG_ANONYMOUS) ? "anonymous " : "",
                (vm->flags & VMAP_FLAG_WRITABLE) ? "writable " : "",
                (vm->flags & VMAP_FLAG_EXEC) ? "executable " : ""
        );

        deliver_segv(vaddr, SEGV_ACCERR); /* does not return */
        assert(0);
    }

    /* vmap, no prot violation --> demand paging */
    return do_demand_page(vm, vaddr);

}

vmap allocate_vmap(rangemap rm, range r, u64 flags)
{
    vmap vm = allocate(rm->h, sizeof(struct vmap));
    if (vm == INVALID_ADDRESS)
        return vm;
    rmnode_init(&vm->node, r);
    vm->flags = flags;
    if (!rangemap_insert(rm, &vm->node)) {
        deallocate(rm->h, vm, sizeof(struct vmap));
        return INVALID_ADDRESS;
    }
    return vm;
}

boolean adjust_vmap_range(rangemap rm, vmap v, range new)
{
    return rangemap_reinsert(rm, &v->node, new);
}

sysreturn mremap(void *old_address, u64 old_size, u64 new_size, int flags, void * new_address)
{
    kernel_heaps kh = get_kernel_heaps();
    process p = current->p;
    u64 old_addr = u64_from_pointer(old_address);

    thread_log(current, "mremap: old_address %p, old_size 0x%lx, new_size 0x%lx, flags 0x%x, new_address %p",
	       old_address, old_size, new_size, flags, new_address);

    if ((flags & MREMAP_MAYMOVE) == 0) {
        msg_err("only supporting MREMAP_MAYMOVE yet\n");
        return -ENOMEM;
    }

    if ((flags & MREMAP_FIXED)) {
        msg_err("no support for MREMAP_FIXED yet\n");
        return -ENOMEM;
    }

    if ((old_addr & MASK(PAGELOG)) ||
        (flags & ~(MREMAP_MAYMOVE | MREMAP_FIXED)) ||
        new_size == 0)
        return -EINVAL;

    /* XXX should determine if we're extending a virtual32 allocation... */
    heap vh = p->virtual_page;
    heap physical = heap_physical(kh);
    heap pages = heap_pages(kh);

    old_size = pad(old_size, vh->pagesize);
    if (new_size <= old_size)
        return sysreturn_from_pointer(old_address);

    /* new virtual allocation */
    u64 maplen = pad(new_size, vh->pagesize);
    u64 vnew = allocate_u64(vh, maplen);
    if (vnew == (u64)INVALID_ADDRESS) {
        msg_err("failed to allocate virtual memory, size %ld\n", maplen);
        return -ENOMEM;
    }

    /* remove old mapping, preserving attributes
     * XXX should verify entire given range
     */
    vmap old_vm = (vmap)rangemap_lookup(p->vmaps, old_addr);
    if (old_vm == INVALID_ADDRESS)
        return -EFAULT;
    u64 vmflags = old_vm->flags;
    rangemap_remove_node(p->vmaps, &old_vm->node);

    /* create new vm with old attributes */
    vmap vm = allocate_vmap(p->vmaps, irange(vnew, vnew + maplen), vmflags);
    if (vm == INVALID_ADDRESS) {
        msg_err("failed to allocate vmap\n");
        deallocate_u64(vh, vnew, maplen);
        return -ENOMEM;
    }

    /* balance of physical allocation */
    u64 dlen = maplen - old_size;
    u64 dphys = allocate_u64(physical, dlen);
    if (dphys == INVALID_PHYSICAL) {
        msg_err("failed to allocate physical memory, size %ld\n", dlen);
        deallocate_u64(vh, vnew, maplen);
        return -ENOMEM;
    }
    thread_log(current, "   new physical pages at 0x%lx, size %ld", dphys, dlen);

    /* remap existing portion */
    thread_log(current, "   remapping existing portion at 0x%lx (old_addr 0x%lx, size 0x%lx)",
               vnew, old_addr, old_size);
    remap_pages(vnew, old_addr, old_size, pages);

    /* map new portion and zero */
    u64 mapflags = page_map_flags(vmflags);
    thread_log(current, "   mapping and zeroing new portion at 0x%lx, page flags 0x%lx",
               vnew + old_size, mapflags);
    map(vnew + old_size, dphys, dlen, mapflags, pages);
    zero(pointer_from_u64(vnew + old_size), dlen);

    return sysreturn_from_pointer(vnew);
}

static CLOSURE_3_3(mincore_fill_vec, boolean, u64, u64, u8 *, int, u64, u64 *);
boolean mincore_fill_vec(u64 base, u64 nr_pgs, u8 * vec, int level, u64 addr, u64 * entry)
{
    u64 e = *entry;
    u64 pgoff, i;

    if (pt_entry_is_present(e)) {
        pgoff = (addr - base) >> PAGELOG;

        if (pt_entry_is_fat(level, e)) {
            /* whole level is mapped */
            for (i = 0; (i < 512) && (pgoff + i < nr_pgs); i++) {
                vec[pgoff + i] = 1;
	    }
        } else if (pt_entry_is_pte(level, e)) {
            vec[pgoff] = 1;
        }
    }

    return true;
}

static CLOSURE_0_1(mincore_vmap_gap, void, range);
static void mincore_vmap_gap(range r)
{
    thread_log(current, "   found gap [0x%lx, 0x%lx)", r.start, r.end);
}

static sysreturn mincore(void *addr, u64 length, u8 *vec)
{
    u64 start, nr_pgs;

    thread_log(current, "mincore: addr %p, length 0x%lx, vec %p",
               addr, length, vec);

    start = u64_from_pointer(addr);
    if (start & MASK(PAGELOG))
        return -EINVAL;

    length = pad(length, PAGESIZE);
    nr_pgs = length >> PAGELOG;

    /* -ENOMEM if any unmapped gaps in range */
    if (rangemap_range_find_gaps(
            current->p->vmaps, 
            (range){start, start + length},
            stack_closure(mincore_vmap_gap)))
        return -ENOMEM;

    runtime_memset(vec, 0, nr_pgs);
    traverse_ptes(start, length,
        stack_closure(mincore_fill_vec, start, nr_pgs, vec)
    );
    return 0;
}

static CLOSURE_6_2(mmap_read_complete, void, thread, u64, u64, boolean, buffer, u64, status, bytes);
static void mmap_read_complete(thread t, u64 where, u64 mmap_len, boolean mapped, buffer b, u64 mapflags,
                               status s, bytes length) {
    if (!is_ok(s)) {
        deallocate_buffer(b);
        set_syscall_error(t, EACCES);
        thread_wakeup(t);
	return;
    }

    kernel_heaps kh = (kernel_heaps)&t->uh;
    heap pages = heap_pages(kh);
    heap physical = heap_physical(kh);

    // mutal misalignment?...discontiguous backing?
    u64 length_padded = pad(length, PAGESIZE);
    u64 p = physical_from_virtual(buffer_ref(b, 0));
    if (mapped) {
        update_map_flags(where, length, mapflags);
        runtime_memcpy(pointer_from_u64(where), buffer_ref(b, 0), length);
    } else {
        map(where, p, length_padded, mapflags, pages);
    }

    if (length < length_padded)
        zero(pointer_from_u64(where + length), length_padded - length);

    if (length_padded < mmap_len) {
        u64 bss = pad(mmap_len, PAGESIZE) - length_padded;
        if (!mapped)
            map(where + length_padded, allocate_u64(physical, bss), bss, mapflags, pages);
        else
            update_map_flags(where + length_padded, bss, mapflags);
        zero(pointer_from_u64(where + length_padded), bss);
    }

    if (mapped) {
        deallocate_buffer(b);
    } else {
        /* XXX This is gross. Either support this within the buffer interface or use something besides
           a buffer... */
        physically_backed_dealloc_virtual(b->h, u64_from_pointer(buffer_ref(b, 0)), pad(mmap_len, b->h->pagesize));
        deallocate(b->h, b, sizeof(struct buffer));
    }

    set_syscall_return(t, where);
    thread_wakeup(t);
}

#if 0
static CLOSURE_0_1(vmap_dump_node, void, rmnode);
static void vmap_dump_node(rmnode n)
{
    vmap curr = (vmap)n;
    rprintf("  %R, %s%s%s%s\n", curr->node.r,
            (curr->flags & VMAP_FLAG_MMAP) ? "mmap " : "",
            (curr->flags & VMAP_FLAG_ANONYMOUS) ? "anonymous " : "",
            (curr->flags & VMAP_FLAG_WRITABLE) ? "writable " : "",
            (curr->flags & VMAP_FLAG_EXEC) ? "exec " : "");
}

static void vmap_dump(rangemap pvmap)
{
    rprintf("vmap %p\n", pvmap);
    rmnode_handler nh = closure(heap_general(get_kernel_heaps()), vmap_dump_node);
    rangemap_range_lookup(pvmap, (range){0, infinity}, nh);
}
#endif

/* XXX refactor */
static CLOSURE_3_1(vmap_attribute_update_intersection, void, heap, rangemap, vmap, rmnode);
static void vmap_attribute_update_intersection(heap h, rangemap pvmap, vmap q, rmnode node)
{
    vmap match = (vmap)node;
    if (vmap_attr_equal(q, match))
        return;

    range rn = node->r;
    range rq = q->node.r;
    range ri = range_intersection(rq, rn);

    boolean head = ri.start > rn.start;
    boolean tail = ri.end < rn.end;

    /*
      case 1: !head && !tail

      node: |----------------|
      q:    |----------------|  (can extend past node start or end)
      attr: qqqqqqqqqqqqqqqqqq

      case 2: head && !tail

      node: |----------------|
      q:       |---------------|
      attr: nnnqqqqqqqqqqqqqqq

      case 3: !head && tail

      node:    |----------------|
      q:    |---------------|
      attr:    qqqqqqqqqqqqqqnnnn

      case 4: head && tail

      node: |----------------|
      q:       |---------|
      attr: nnnqqqqqqqqqqqnnnn

    */

    u64 newflags = (match->flags & ~(VMAP_FLAG_WRITABLE | VMAP_FLAG_EXEC)) | q->flags;

    if (head) {
        u64 rtend = rn.end;

        /* split non-intersecting part of node */
        range rhl = { rn.start, ri.start };
        assert(rangemap_reinsert(pvmap, node, rhl));

        /* create node for intersection */
        vmap mh = allocate_vmap(pvmap, ri, newflags);
        assert(mh != INVALID_ADDRESS);
        
        if (tail) {
            /* create node at tail end */
            range rt = { ri.end, rtend };
            vmap mt = allocate_vmap(pvmap, rt, match->flags);
            assert(mt != INVALID_ADDRESS);
        }
    } else if (tail) {
        /* move node start back */
        range rt = { ri.end, node->r.end };
        assert(rangemap_reinsert(pvmap, node, rt));

        /* create node for intersection */
        vmap mt = allocate_vmap(pvmap, ri, newflags);
        assert(mt != INVALID_ADDRESS);
    } else {
        /* key (range) remains the same, no need to reinsert */
        match->flags = newflags;
    }
}

static void vmap_attribute_update(heap h, rangemap pvmap, vmap q)
{
    range rq = q->node.r;
    assert((rq.start & MASK(PAGELOG)) == 0);
    assert((rq.end & MASK(PAGELOG)) == 0);
    assert(range_span(rq) > 0);

    rmnode_handler nh = stack_closure(vmap_attribute_update_intersection, h, pvmap, q);
    rangemap_range_lookup(pvmap, rq, nh);

    update_map_flags(rq.start, range_span(rq), page_map_flags(q->flags));
}

sysreturn mprotect(void * addr, u64 len, int prot)
{
    thread_log(current, "mprotect: addr %p, len 0x%lx, prot 0x%x", addr, len, prot);

    if (len == 0)
        return 0;

    heap h = heap_general(get_kernel_heaps());
    rangemap pvmap = current->p->vmaps;
    u64 where = u64_from_pointer(addr);
    u64 padlen = pad(len, PAGESIZE);
    if ((where & MASK(PAGELOG)))
        return -EINVAL;

    u64 new_vmflags = 0;
    if ((prot & PROT_EXEC))
        new_vmflags |= VMAP_FLAG_EXEC;
    if ((prot & PROT_WRITE))
        new_vmflags |= VMAP_FLAG_WRITABLE;

    range r = { where, where + padlen };
    struct vmap q;
    q.node.r = r;
    q.flags = new_vmflags;

    vmap_attribute_update(h, pvmap, &q);
    return 0;
}

/* If we can re-use a node (range is exact match), just update the
   flags. Otherwise trim back any vmap(s) that overlap with q,
   creating a new node if necessary. After this point, new nodes can
   be created using the gap fill.
 */
static CLOSURE_3_1(vmap_paint_intersection, void, heap, rangemap, vmap, rmnode);
static void vmap_paint_intersection(heap h, rangemap pvmap, vmap q, rmnode node)
{
    vmap match = (vmap)node;
    if (vmap_attr_equal(q, match))
        return;

    range rn = node->r;
    range ri = range_intersection(q->node.r, rn);

    assert(range_span(ri) > 0); // XXX

    /* attribute mismatch; check if we can just reuse matching node */
    if (range_equal(ri, rn)) {
        /* key (range) remains the same, no need to reinsert */
        match->flags = q->flags;
        return;
    }

    /* trim match at both head and tail ends */
    boolean head = ri.start > rn.start;
    boolean tail = ri.end < rn.end;

    if (head) {
        /* truncate node at start */
        range rh = { rn.start, ri.start };
        u64 rtend = rn.end;
        rangemap_reinsert(pvmap, node, rh);

        if (tail) {
            /* create node at tail end */
            range rt = { ri.end, rtend };
            vmap mt = allocate_vmap(pvmap, rt, match->flags);
            assert(mt != INVALID_ADDRESS);
        }
    } else if (tail) {
        /* move node start back */
        range rt = { ri.end, rn.end };
        rangemap_reinsert(pvmap, node, rt);
    }
}

static CLOSURE_3_1(vmap_paint_gap, void, heap, rangemap, vmap, range);
static void vmap_paint_gap(heap h, rangemap pvmap, vmap q, range r)
{
    vmap mt = allocate_vmap(pvmap, r, q->flags);
    assert(mt != INVALID_ADDRESS);
}

static void vmap_paint(heap h, rangemap pvmap, vmap q)
{
    range rq = q->node.r;
    assert((rq.start & MASK(PAGELOG)) == 0);
    assert((rq.end & MASK(PAGELOG)) == 0);
    assert(range_span(rq) > 0);

    rmnode_handler nh = stack_closure(vmap_paint_intersection, h, pvmap, q);
    rangemap_range_lookup(pvmap, rq, nh);

    range_handler rh = stack_closure(vmap_paint_gap, h, pvmap, q);
    rangemap_range_find_gaps(pvmap, rq, rh);

    update_map_flags(rq.start, range_span(rq), page_map_flags(q->flags));
}

typedef struct varea {
    struct rmnode node;
    heap h;
    boolean allow_fixed;
} * varea;

static varea allocate_varea(heap h, rangemap vareas, range r, heap vh, boolean allow_fixed)
{
    varea va = allocate(h, sizeof(struct varea));
    if (va == INVALID_ADDRESS)
        return va;
    rmnode_init(&va->node, r);
    va->allow_fixed = allow_fixed;
    va->h = vh;
    if (!rangemap_insert(vareas, &va->node)) {
        deallocate(h, va, sizeof(struct vmap));
        return INVALID_ADDRESS;
    }
    return va;
}

/* XXX defaulting to leniency; revisit */
static boolean mmap_reserve_range(process p, range q)
{
    /* XXX can tweak rangemap range lookup to terminate if a callback
       fails...kind of tired of messing with that interface */
    varea a = (varea)rangemap_first_node(p->vareas);
    while (a != INVALID_ADDRESS) {
        if (ranges_intersect(q, a->node.r)) {
            if (!a->allow_fixed)
                return false;
            if (a->h)
                id_heap_set_area(a->h, q.start, range_span(q), false, true);
        }
        a = (varea)rangemap_next_node(p->vareas, (rmnode)a);
    }
    return true;
}

static sysreturn mmap(void *target, u64 size, int prot, int flags, int fd, u64 offset)
{
    process p = current->p;
    kernel_heaps kh = get_kernel_heaps();
    heap h = heap_general(kh);
    u64 len = pad(size, PAGESIZE) & MASK(32);
    boolean mapped = false;
    thread_log(current, "mmap: target %p, size 0x%lx, prot 0x%x, flags 0x%x, fd %d, offset 0x%lx",
	       target, size, prot, flags, fd, offset);

    /* Determine vmap flags */
    u64 vmflags = VMAP_FLAG_MMAP;
    if ((flags & MAP_ANONYMOUS))
        vmflags |= VMAP_FLAG_ANONYMOUS;
    if ((prot & PROT_EXEC))
        vmflags |= VMAP_FLAG_EXEC;
    if ((prot & PROT_WRITE))
        vmflags |= VMAP_FLAG_WRITABLE;

    /* Don't really try to honor a hint, only fixed. */
    boolean fixed = (flags & MAP_FIXED) != 0;
    u64 where = fixed ? u64_from_pointer(target) : 0;

    if (fixed) {
        if (where == 0) {
            thread_log(current, "   attempt to map zero page");
            return -ENOMEM;
        }

	/* Must be page-aligned */
	if (where & MASK(PAGELOG)) {
	    thread_log(current, "   attempt to map non-aligned FIXED address");
	    return -EINVAL;
	}

        /* A specified address is only allowed in certain areas. Programs may specify
           a fixed address to augment some existing mapping. */
        range q = irange(where, where + len);
        if (!mmap_reserve_range(p, q)) {
	    thread_log(current, "   fail: fixed address range %R outside of lowmem or virtual_page heap\n", q);
	    return -ENOMEM;
        }
    }

    /* Allocate from virtual heap if no address specified or hint unavailable */
    if (where == 0) {
        boolean is_32bit = flags & MAP_32BIT;
        u64 maplen = pad(len, PAGESIZE);
        if (is_32bit) {
            /* Allocate from top half of 32-bit address space. */
            where = id_heap_alloc_subrange(p->virtual32, maplen, 0x80000000, 0x100000000);
        } else {
            where = allocate_u64(p->virtual_page, maplen);
        }
        if (where == (u64)INVALID_ADDRESS) {
            /* We'll always want to know about low memory conditions, so just bark. */
            msg_err("failed to allocate %s virtual memory, size 0x%lx\n", is_32bit ? "32-bit" : "", len);
            return -ENOMEM;
        }
    }

    /* Paint into process vmap */
    struct vmap q;
    q.flags = vmflags;
    q.node.r = irange(where, where + len);
    vmap_paint(h, p->vmaps, &q);

    if (flags & MAP_ANONYMOUS) {
        thread_log(current, "   anon target: %s, 0x%lx, len: 0x%lx (given size: 0x%lx)",
                   mapped ? "existing" : "new", where, len, size);
        zero_mapped_pages(where, len);
        return where;
    }

    file f = resolve_fd(current->p, fd);
    thread_log(current, "  read file at 0x%lx, %s map, blocking...", where, mapped ? "existing" : "new");

    heap mh = heap_backed(kh);
    bytes blen = pad(len, mh->pagesize);
    buffer b = allocate_buffer(mh, mh, allocate(mh, blen), blen);
    filesystem_read(p->fs, f->n, buffer_ref(b, 0), len, offset,
                    closure(h, mmap_read_complete, current, where, len, mapped, b, page_map_flags(vmflags)));
    thread_sleep_uninterruptible();
}

static CLOSURE_1_1(dealloc_phys_page, void, heap, range);
static void dealloc_phys_page(heap physical, range r)
{
    if (!id_heap_set_area(physical, r.start, range_span(r), true, false))
        msg_err("some of physical range %R not allocated in heap\n", r);
}

static CLOSURE_2_1(process_unmap_intersection, void, process, range, rmnode);
static void process_unmap_intersection(process p, range rq, rmnode node)
{
    kernel_heaps kh = get_kernel_heaps();
    vmap match = (vmap)node;
    range rn = node->r;
    range ri = range_intersection(rq, rn);

    /* similar logic to attribute update above */
    boolean head = ri.start > rn.start;
    boolean tail = ri.end < rn.end;

//    rprintf("unmap q %R, node %R, head %d, tail %d\n", rq, node->r, head, tail);

    if (head) {
        u64 rtend = rn.end;

        /* split non-intersecting part of node */
        range rhl = { rn.start, ri.start };
        assert(rangemap_reinsert(p->vmaps, node, rhl));

        if (tail) {
            /* create node for tail end */
            range rt = { ri.end, rtend };
            vmap mt = allocate_vmap(p->vmaps, rt, match->flags);
            assert(mt != INVALID_ADDRESS);
        }
    } else if (tail) {
        /* move node start back */
        range rt = { ri.end, node->r.end };
        assert(rangemap_reinsert(p->vmaps, node, rt));
    } else {
        /* delete outright */
        rangemap_remove_node(p->vmaps, node);
    }

    /* unmap any mapped pages and return to physical heap */
    u64 len = range_span(ri);
    unmap_pages_with_handler(ri.start, len, stack_closure(dealloc_phys_page, heap_physical(kh)));

    /* return virtual mapping to heap, if any ... assuming a vmap cannot span heaps!
       XXX: this shouldn't be a lookup per, so consider stashing a link to varea or heap in vmap
       though in practice these are small numbers... */
    varea v = (varea)rangemap_lookup(p->vareas, ri.start);
    assert(v != INVALID_ADDRESS);
    if (v->h)
        id_heap_set_area(v->h, ri.start, len, false, false);
}

static void process_unmap_range(process p, range q)
{
    rmnode_handler nh = stack_closure(process_unmap_intersection, p, q);
    rangemap_range_lookup(p->vmaps, q, nh);
}

static sysreturn munmap(void *addr, u64 length)
{
    process p = current->p;
    thread_log(current, "munmap: addr %p, size 0x%lx", addr, length);

    u64 where = u64_from_pointer(addr);
    if ((where & MASK(PAGELOG)) || length == 0)
        return -EINVAL;

    u64 padlen = pad(length, PAGESIZE);
    range q = irange(where, where + padlen);

    /* clear out any mapped areas in our meta */
    process_unmap_range(p, q);
    return 0;
}

/* kernel start */
extern void * START;

static void add_varea(process p, u64 start, u64 end, heap vheap, boolean allow_fixed)
{
    assert(allocate_varea(heap_general((kernel_heaps)p->uh), p->vareas, irange(start, end),
                          vheap, allow_fixed) != INVALID_ADDRESS);

    /* reserve area by marking as allocated */
    if (vheap && !allow_fixed)
        id_heap_set_area(vheap, start, end - start, true, true);
}

void mmap_process_init(process p)
{
    kernel_heaps kh = &p->uh->kh;
    heap h = heap_general(kh);
    range identity_map = irange(kh->identity_reserved_start, kh->identity_reserved_end);
    p->vareas = allocate_rangemap(h);
    p->vmaps = allocate_rangemap(h);
    assert(p->vareas != INVALID_ADDRESS && p->vmaps != INVALID_ADDRESS);

    /* zero page is off-limits */
    add_varea(p, 0, PAGESIZE, p->virtual32, false);

    /* as is the identity heap */
    add_varea(p, identity_map.start, identity_map.end, p->virtual32, false);

    /* and kernel */
    add_varea(p, KERNEL_RESERVE_START, KERNEL_RESERVE_END, p->virtual32, false);

    /* but explicitly allow any maps in between */
    p->lowmem_end = MIN(KERNEL_RESERVE_START, identity_map.start);
    add_varea(p, PAGESIZE, p->lowmem_end, p->virtual32, true);

    /* reserve kernel huge page area */
    add_varea(p, HUGE_PAGESIZE, PROCESS_VIRTUAL_HEAP_START, 0, false);

    /* allow (tracked) reservations in p->virtual */
    add_varea(p, PROCESS_VIRTUAL_HEAP_START, PROCESS_VIRTUAL_HEAP_END, p->virtual_page, true);

    /* reserve end of p->virtual to user tag region */
    u64 user_va_tag_start = U64_FROM_BIT(user_va_tag_offset);
    u64 user_va_tag_end = user_va_tag_start * tag_max;
    add_varea(p, PROCESS_VIRTUAL_HEAP_END, user_va_tag_start, 0, false);

    /* allow untracked mmaps in user va tag area */
    add_varea(p, user_va_tag_start, user_va_tag_end, 0, true);

    /* reserve user va tag area from kernel perspective */
    assert(id_heap_set_area(heap_virtual_huge(kh), user_va_tag_start, user_va_tag_end, true, true));

    /* reserve remainder */
    add_varea(p, user_va_tag_end, U64_FROM_BIT(VIRTUAL_ADDRESS_BITS), 0, false);

    /* Track vsyscall page */
    assert(allocate_vmap(p->vmaps, irange(VSYSCALL_BASE, VSYSCALL_BASE + PAGESIZE), VMAP_FLAG_EXEC));
}

void register_mmap_syscalls(struct syscall *map)
{
    register_syscall(map, mincore, mincore);
    register_syscall(map, mmap, mmap);
    register_syscall(map, mremap, mremap);
    register_syscall(map, munmap, munmap);
    register_syscall(map, mprotect, mprotect);
    register_syscall(map, madvise, syscall_ignore);
}
