#include <runtime.h>
#include <net.h>
#include <http.h>

extern  char **_binary_management_js_js_start;
extern  u64 _binary_management_js_js_size;

closure_function(1, 2, void, each,
                 buffer, b,
                 value, k,
                 value, v)
{
    rprintf("keyo %b %b\n", k, v);
}
                 
                 
buffer subkeys(heap h, value m)
{
    buffer b = allocate_buffer(h, 100);
    switch (tagof(m)) {
    case tag_tuple:
        table_foreach(m, k, v) 
            bprintf(b, "%v ", k);
        break;
    case tag_function_tuple:
        apply(((function_tuple)m)->i, closure(h, each, b));
        break;
    }
    return b;
}

// per request heap
closure_function(3, 1, void, each_http_request,
                 heap, h,
                 tuple, root,
                 buffer_handler, out,
                 value, v)
{
    heap h = bound(h);
    vector vsl = vector_from_tuple(h, table_find(v, sym(start_line)));
    buffer url = vector_get(vsl, 1);
    vector terms = split(h, url, '/');
    buffer i;
    value where = bound(root);
    
    vector_foreach(terms, i) { // backwards, non-declaring
        if (buffer_length(i)) { // really only the first one
            where = get(where, intern(i));
        }
    }
    buffer b = where;
    if (tagof(where) != tag_string) {
        b = subkeys(h, where);
    }
    send_http_response(bound(out), timm("Content-Type", "text/html"), b);
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
    rprintf("management: %x %x\n",_binary_management_js_js_start,
            _binary_management_js_js_size);
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
