#include <runtime.h>
#include <net.h>
#include <http.h>

// per request heap
closure_function(3, 1, void, each_http_request,
                 heap, h,
                 tuple, root,
                 buffer_handler, out,
                 value, v)
{
    rprintf("http: %v\n", v);
    send_http_response(bound(out), timm("Content-Type", "text/html"),
                       aprintf(bound(h),
                               "<html>unibooty!</html>"));
}

closure_function(2, 1, buffer_handler, each_http_connection,
                 heap, h,
                 tuple, root, 
                 buffer_handler, out)
{
    heap h = bound(h);
    return allocate_http_parser(h, closure(h, each_http_request, h, bound(root), out));
}

void init_management_http(heap h, tuple root) {
    string r = get(root, sym(management_http));
    u64 port;
    if (r && parse_int(r, 10, &port)) {
        // and presumably not truncation
        rprintf("management http %d\n", port);
        http_listener hl = allocate_http_listener(h, port);
        assert(hl != INVALID_ADDRESS);
        
        status s = listen_port(h, port, closure(h, each_http_connection, h, root));
        if (!is_ok(s))
            halt("listen_port failed for http listener: %v\n", s);
        rprintf("http management server started on port %d\n", port);
    } else {
        rprintf("no m\n");
    }
}
