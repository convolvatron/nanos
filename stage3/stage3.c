#include <runtime.h>
#include <tfs.h>
#include <unix.h>
#include <gdb.h>
#include <virtio/virtio.h>
#include <http.h>
#include <net.h>

static CLOSURE_2_2(read_program_complete, void, process, tuple, buffer, thunk);
static void read_program_complete(process kp, tuple root, buffer b, thunk k)
{
    if (table_find(root, sym(trace))) {
        rprintf("read program complete: %p ", root);
        rprintf("gitversion: %s ", gitversion);

        /* XXX - disable this until we can be assured that print_root
           won't go haywire on a large manifest... */
#if 0
        buffer b = allocate_buffer(transient, 64);
        print_root(b, root);
        buffer_print(b);
        deallocate_buffer(b);
        rprintf("\n");
#endif
       
    }
    exec_elf(b, kp);
}

static CLOSURE_0_1(read_program_fail, void, status);
static void read_program_fail(status s)
{
    halt("read program failed %v\n", s);
}

static CLOSURE_2_1(each_request, void, heap, buffer_handler, value);
static void each_request(heap h, buffer_handler out, value v)
{
    send_http_response(out,
                       timm("ContentType", "text/html"),
                       aprintf(h, "unibooty!"));
}

static CLOSURE_1_1(each_connection, buffer_handler, heap, buffer_handler);
static buffer_handler each_connection(heap h, buffer_handler out)
{
    return allocate_http_parser(h, closure(h, each_request, h, out));
}

void startup(kernel_heaps kh,
             tuple root,
             filesystem fs)
{
    /* kernel process is used as a handle for unix */
    process kp = init_unix(kh, root, fs);
    if (kp == INVALID_ADDRESS) {
	halt("unable to initialize unix instance; halt\n");
    }
        
    init_network_iface(root);

    heap general = heap_general(kh);
    buffer_handler pg = closure(general, read_program_complete, kp, root);
    if(table_find(root, sym(http))) {
        // configure port
        listen_port(general, 8080, closure(general, each_connection, general));
        rprintf("Server started on port %d\n", 8080);        
    }
    
    value p = table_find(root, sym(program));
    if (p) {
        tuple pro = resolve_path(root, split(general, p, '/'));
        filesystem_read_entire(fs, pro, heap_backed(kh), pg, closure(general, read_program_fail));
    }
}

