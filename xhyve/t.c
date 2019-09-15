typedef unsigned char uint8_t;
typedef unsigned long long uint64_t;
typedef uint64_t size_t;
typedef unsigned long u_long;
typedef unsigned int u_int;
typedef u_int uint32_t;


#include <userboot.h>
#include <runtime.h>
#include <tfs.h>
#include <region.h>
#include <elf64.h>

struct loader_callbacks *cb;
void *arg;

// xxx - some boilerplate shared with stage2
region fsregion()
{
    for_regions(r) {
        if (r->type == REGION_FILESYSTEM)
            return r;
    }
    halt("invalid filesystem offset\n");
}

static CLOSURE_0_3 (fzero, void, void *, u64, u64);
static void fzero(void *a,  u64 b , u64 c)
{
    // implicit zero? - leftover scraps of pages?
}

static CLOSURE_0_4(kernel_elf_map, void, u64, u64, u64, u64);
// source should always be void *
static void kernel_elf_map(u64 target, u64 source, u64 size, u64 flags)
{
    if (source == INVALID_PHYSICAL) {
        zero(pointer_from_u64(target), size);
    } else {
        cb->copyin(arg, pointer_from_u64(source), target, size);
    }
}

static CLOSURE_0_1(kernel_read_complete, void, buffer);
static void __attribute__((noinline)) kernel_read_complete(buffer kb)
{
    /* save kernel elf image for use in stage3 (for symbol data) */
    create_region(u64_from_pointer(buffer_ref(kb, 0)), pad(buffer_length(kb), PAGESIZE), REGION_KERNIMAGE);
    void *k = load_elf(kb, 0, stack_closure(kernel_elf_map), stack_closure(fzero));
    cb->exec(arg, u64_from_pointer(k));
}

CLOSURE_0_1(fail, void, status);
void fail(status s)
{
    halt("filesystem_read_entire failed: %v\n", s);
}

static CLOSURE_0_3(stage2_empty_write, void, void *, range, status_handler);
static void stage2_empty_write(void * src, range blocks, status_handler completion)
{
}

static CLOSURE_1_3(userboot_block_read, void, u64, void *, range, status_handler);
static void userboot_block_read(u64 offset, void *dest, range blocks, status_handler completion)
{
}
    
static CLOSURE_3_2 (filesystem_initialized, void, heap, tuple, buffer_handler, filesystem, status);
static void filesystem_initialized(heap h, tuple root, buffer_handler complete, filesystem fs, status s)
{
    filesystem_read_entire(fs,
                           lookup(root, sym(kernel)),
                           h,
                           complete, 
                           closure(h, fail));
}


void loader_main(struct loader_callbacks *lcb, void *larg, int version, int ndisks)
{
    cb = lcb;
    arg = larg;
    heap h = init_process_runtime();
    buffer_handler bh = closure(h, kernel_read_complete);
    tuple root = allocate_tuple();
    
    u32 fs_offset = SECTOR_SIZE + fsregion()->length; // MBR + stage2
    // set memory region
    create_filesystem(h,
                      SECTOR_SIZE,
                      infinity,
                      0,         /* ignored in boot */
                      closure(h, userboot_block_read, fs_offset),
                      closure(h, stage2_empty_write),
                      root,
                      closure(h, filesystem_initialized, h, root, bh));    
    cb->exit(arg, 0);
}
