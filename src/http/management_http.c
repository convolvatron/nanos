#include <runtime.h>
#include <net.h>
#include <http.h>

closure_function(1, 3, void, each_request,
                 heap, h,
                 http_method, m,
                 buffer_handler, out,
                 value, v)
{
    heap h = bound(h);
    rprintf("http: %s request via http: %v\n", http_request_methods[m], v);
    buffer u = table_find(v, sym(relative_uri));
    rprintf("foo: %b\n", u);
    //        vector path = split(h, u, '/');        

    send_http_response(out, timm("Content-Type", "text/html"), aprintf(h, "<html>unibooty!</html>"));
    return;

}

void init_management_http(heap h, tuple root) {
    string r;
    rprintf("init m\n");
    if ((r = get(root, sym(management)))) {
        u16 port = parse_int(r, 10, 0);
        rprintf("init m %d\n", port);
        http_listener hl = allocate_http_listener(h, port);
        assert(hl != INVALID_ADDRESS);
        http_register_uri_handler(hl, "", closure(h, each_request, h));
        
        status s = listen_port(h, port, connection_handler_from_http_listener(hl));
        if (!is_ok(s))
            halt("listen_port failed for http listener: %v\n", s);
        rprintf("http management server started on port %d\n", port);
    }
}
