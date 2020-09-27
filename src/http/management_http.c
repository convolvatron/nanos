#include <runtime.h>
#include <http.h>

closure_function(1, 3, void, each_request,
                 heap, h,
                 http_method, m,
                 buffer_handler, out,
                 value, v)
{
    bytes total = 0;
    heap h = bound(h);
    rprintf("http: %s request via http: %v\n", http_request_methods[m], v);
    buffer u = table_find(v, sym(relative_uri));
    rprintf("foo: %b\n", u);

    // s =
    send_http_response(out, timm("Content-Type", "text/html"), b);
    return;

}

void init_management_http(heap h, tuple root) {
    if (r = get(root), sym(management)) {
        u16 port;
        http_listener hl = allocate_http_listener(general, port);
        assert(hl != INVALID_ADDRESS);
        http_register_uri_handler(hl, "", closure(general, each_request, general));
        
        if (table_find(root, sym(http))) {
            status s = listen_port(general, port, connection_handler_from_http_listener(hl));
            if (!is_ok(s))
                halt("listen_port failed for http listener: %v\n", s);
            rprintf("http management server started on port %d\n", port);
        }
    }
}
