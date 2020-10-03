#include <runtime.h>
#include <net.h>
#include <http.h>

extern  char **_binary_management_js_js_start;
extern  u64 _binary_management_js_js_size;
static buffer management_js;

typedef struct session {
    buffer_handler out;
} *session;

closure_function(1, 2, void, each,
                 buffer, b,
                 value, k,
                 value, v)
{
    rprintf("keyo %b %b\n", k, v);
    //serialize(b, 
    //              timm(sym(text),
    //                   timm(sym(body), b,
    //                        sym(callback), aprintf(h, "poppy"))));
}


// move to runtime
buffer subkeys(heap h, value m, value_handler e)
{
    switch (tagof(m)) {
    case tag_tuple:
        table_foreach(m, k, _)
            apply(e, k);
        break;
    case tag_function_tuple:
        apply(((function_tuple)m)->i, e);
        break;
    }
    return b;
}

closure_function(1, 1, status, each_ws, session, s, buffer, b) {
    rprintf("from websocket %b\n", b);
    return STATUS_OK;
}
    
// per request heap
closure_function(4, 1, void, each_http_request,
                 heap, h,
                 buffer_handler *, director_bh,
                 tuple, root,
                 buffer_handler, out,
                 value, v)
{
    heap h = bound(h);
    buffer_handler out = bound(out);
    rprintf ("url %t\n", v);    
    buffer url = get(get(v, sym(start_line)), sym(1));
    rprintf ("url %b\n", url);    
    vector terms = split(h, url, '/');
    int index = 0;
    value where = bound(root);


    // leading slash
    buffer first_term = vector_get(terms, index);
    if (!first_term || (buffer_length(first_term) == 0)) index++;
    

    if (index == vector_length(terms)) {
        // start up websocket
        buffer up  = get(v, sym(Upgrade));
        // xxx - the browser signatures differ slightly here
        if (up && !buffer_compare(up, symbol_string(sym("websocket")))) {
            session s = allocate(h, sizeof(struct session));
            buffer_handler in;
            s->out = websocket_send_upgrade(h, v, out,
                                            &in,
                                            closure(h, each_ws, s)); 
            *bound(director_bh) = in;
        } else {
            // root - serve up js app
            // surely x-javascript isn't the correct mimetype anymore
            management_js = alloca_wrap_buffer(&_binary_management_js_js_start,
                                               (unsigned long)&_binary_management_js_js_size);
            
            send_http_response(out,
                               timm("Content-Type", "application/x-javascript"),
                               management_js);
        }
    } else {
        // http method interface needs work 
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


closure_function(1, 1, status, director,
                 buffer_handler *, bh,
                 buffer, b)
{
    return apply(*bound(bh), b);
}

closure_function(2, 1, buffer_handler, each_http_connection,
                 heap, h,
                 tuple, root, 
                 buffer_handler, out)
{
    heap h = bound(h);
    buffer_handler *bh = allocate(h, sizeof(buffer_handler *));
    *bh = allocate_http_parser(h, closure(h, each_http_request, h, bh, bound(root), out));
    return closure(h, director, bh);
}

void sha1(buffer, buffer);

void init_management_http(heap h, tuple root) {
    string r = get(root, sym(management_http));
    u64 port;
        
    if (r && parse_int(r, 10, &port)) {
        // xxx - check to make sure there weren't high order bits set
        rprintf("management http %d\n", port);
        http_listener hl = allocate_http_listener(h, port);
        assert(hl != INVALID_ADDRESS);
        
        status s = listen_port(h, port, closure(h, each_http_connection, h, root));
        if (!is_ok(s))
            halt("listen_port failed for http listener: %v\n", s);
        rprintf("http management server started on port %d\n", port);
    }
}
