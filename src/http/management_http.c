#include <runtime.h>
#include <net.h>
#include <http.h>

extern  char **_binary_management_js_js_start;
extern  u64 _binary_management_js_js_size;
static buffer management_js;

typedef struct session {
    tuple root;
    buffer_handler out;
} *session;

// move to runtime
void subkeys(value m, binding_handler e)
{
    switch (tagof(m)) {
    case tag_tuple:
        table_foreach(m, k, v)
            apply(e, k, v);
        break;
    case tag_function_tuple:
        apply(((function_tuple)m)->i, e);
        break;
    }
}

// still a little uncomfortable with (a (b (c ...))) as a path abstraction,
// but it merges well (?). so anyways, as a result this appends an element
// to all the contained trees

// copying - really consider interning them all
tuple path_append(tuple n, value e) {
    if (!table_elements(n)) {
        return e;
    } 
    tuple t = allocate_tuple();
    table_foreach(n, k, v) 
        table_set(t, k, path_append(v, e));
    return t;        
}

closure_function(3, 2, void, add_node,
                 tuple, path, // we're going with nested tuple paths today
                              // instead of vector or path string
                 tuple, dest,
                 u64 *, offset,
                 value, name,
                 value, _)
{
    u64 *offset = bound(offset);
    string y = allocate_string();
    bprintf(y, "%d", *offset*15 + 10);
    rprintf("z %p %d %v\n", name, tagof(name), y);
    
    //for the moment, we cant trust tree producers to use buffers with
    // the string tag. and symbols come in here? make this stuff work in
    // general
    string ns = allocate_string();
    bprintf(ns, "%v", name);

    // is this proper deletion?
    // should we use unique names since there is a race on deleting the
    // old one and instantiating the new one?
    // we should style directories and leaves differently
    // tuple_union(delete, newpanel)
    
    tuple action = timm("ui",timm("panel",allocate_tuple()),
                        "generate", path_append(bound(path), ns));
    buffer a = aprintf(transient, "z %v\n", action);
    buffer_write_byte(a, 0);
    console(a->contents);
    
    tuple z = timm("kind", "text",
                   "x", "10", "y", y, "text", ns, "click", action);

    buffer n = little_stack_buffer(20);
    buffer_write_byte(n, *offset + 'a');
    table_set(bound(dest), intern(n), z);
    
    // layout?
    *offset = *offset +1;
}

// there is a vector_resolve path in tfs
tuple tuple_resolve_path(tuple where, tuple path)
{
    if (!table_elements(path)) 
        return where;

    table_foreach(path, k, v) {
        tuple x = table_find(where, k);
        if (!x) return x;
        return resolve_path(x, v);
    }
    return 0;
}

tuple generate_panel(table root, tuple path)
{
    // pacc anyone?
    tuple panel_children = allocate_tuple();        
    tuple panel = timm("kind", "g", "children", panel_children);
    tuple children = timm("panel", panel);
    tuple dest = timm("children", children);
    
    u64 *offset = allocate(transient, sizeof(u64));
    *offset = 0;
    subkeys(tuple_resolve_path(root, path),
            closure(transient, add_node, path, panel_children, offset));
    rprintf("%t\n", dest);
    return dest;
}

closure_function(1, 1, status, each_ws, session, s, buffer, b) {
    session s = bound(s);
    if (b) {
        rprintf("from websocket %b\n", b);
        buffer out = allocate_buffer(transient, 100);
        format_json(out, generate_panel(bound(s)->root, allocate_tuple()));
        apply(s->out, out);
    } else {
        rprintf("websocket closed\n");
    }
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
    buffer url = get(get(v, sym(start_line)), sym(1));
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
            s->root = bound(root);
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
                //                b = subkeys(where);
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
        http_listener hl = allocate_http_listener(h, port);
        assert(hl != INVALID_ADDRESS);
        
        status s = listen_port(h, port, closure(h, each_http_connection, h, root));
        if (!is_ok(s))
            halt("listen_port failed for http listener: %v\n", s);
        rprintf("http management server started on port %d\n", port);
    }
}
