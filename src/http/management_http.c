#include <runtime.h>
#include <net.h>
#include <http.h>

extern  char **_binary_management_js_js_start;
extern  u64 _binary_management_js_js_size;
static buffer management_js;

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
    buffer url = get(get(v, sym(start_line)), sym(1));
    vector terms = split(h, url, '/');
    int index = 0;
    value where = bound(root);

    // leading slash
    buffer first_term = vector_get(terms, index);
    if (!first_term || (buffer_length(first_term) == 0)) index++;

    rprintf("index: %d %d\n", index, vector_length(terms));
    // root - serve up js app
    if (index == vector_length(terms)) {
        // surely x-javascript isn't the correct mimetype anymore
        send_http_response(bound(out),
                           timm("Content-Type", "application/x-javascript"),
                           management_js);

    } else {
        // tree
        if (buffer_compare_with_cstring(vector_get(terms,index), "tree")) {
            index++;
            while (where && index < vector_length(terms)) {
                buffer term = vector_get(terms, index);
                where = get(where, intern(term));
            }
            // return 404
            buffer b = where;
            if (tagof(where) != tag_string) {
                b = subkeys(h, where);
            }
            send_http_response(bound(out), timm("Content-Type", "text/html"), b);
        }
    }
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

    management_js = wrap_buffer(h, &_binary_management_js_js_start,
                                (unsigned long)&_binary_management_js_js_size);
    if (r && parse_int(r, 10, &port)) {
        // xxx - check to make sure there weren't high order bits set
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
