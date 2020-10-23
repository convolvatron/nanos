#include <runtime.h>
#include <net.h>
#include <http.h>

extern char **_binary_management_js_js_start;
extern u64 _binary_management_js_js_size;
static buffer management_js;

typedef struct session {
    tuple root;
    buffer_handler out;
    heap h;
} *session;


// variadic set if this lives
value json_write(value n, value v)
{
    tuple body = timm("name", n, "value", v);
    return timm("write", body);
}

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
// this isn't returning a tuple path at all..
tuple path_append(tuple n, value e) {
    if (!table_elements(n)) {
        return e;
    } 
    tuple t = allocate_tuple();
    table_foreach(n, k, v) 
        table_set(t, k, path_append(v, e));
    return t;        
}


static void add_directory_entry(tuple path,
                            tuple dest,
                            u64 * offset,
                            value name)
{
    tuple zero_g = timm("0", "generate");
    string y = allocate_string();
    bprintf(y, "%d", *offset*15 + 10);
    
    tuple z = timm("kind", "text", "x", "10", "y", y, "text", name,
                   "click", json_write(zero_g, path));
    // probably has to be escaped a bit
    table_set(dest, intern(name), z); 
    *offset = *offset +1;     // layout?
}

static void add_value_entry(tuple dest,
                            u64 * offset,
                            value name,
                            value v)
{
    string y = allocate_string();
    bprintf(y, "%d", *offset*15 + 10);
        
    tuple nt = timm("kind", "text", "x", "10", "y", y, "text", name);
    table_set(dest, intern(name), nt);

    // assuming its a string!
    void *value  =v;
    if (tagof(value) == tag_unknown) {
        if (value == ((void *)true)) {
            value = sstring("true");
        } else {
            value =  "<unknown type>";
        }     
    }

    tuple vt = timm("kind", "text", "x", "140", "y", y, "text", value);
    // this raises namespace conflicts
    table_set(dest, intern(aprintf(transient, "%v-value", name)), vt);
    // add subscription tracking
    //    table_set(subs, intern(aprintf(transient, "subscription", path, name))",
    *offset = *offset +1;     // layout?
}
    
closure_function(3, 2, void, add_node,
                 vector, path,
                 tuple, dest,
                 u64 *, offset,
                 value, name,
                 value, v)
{
    // should subscribe to 'path'
    
    //for the moment, we cant trust tree producers to use buffers with
    // the string tag. and symbols come in here? make this stuff work in
    // general
    string ns = allocate_string();
    bprintf(ns, "%v", name);

    
    // read or write?
    tuple p = tuple_from_vector(bound(path));
    tuple_vector_push(p, ns);

    // function tuple isn't necessarily a tuple? can it be a leaf?
    // what if this changes?
    
    if ((tagof(v) == tag_tuple) || (tagof(v) == tag_function_tuple)) {
        add_directory_entry(p, bound(dest), bound(offset), ns);
    } else {
        add_value_entry(bound(dest), bound(offset), ns, v);
    }
}


//    tuple children = timm("panel", panel);
//    tuple dest = timm("children", children);
/* asdf subscription - we can generate the retraction - or just*/
// blow it all away

tuple generate_panel(heap h, table root, vector path)
{
    // decla-pacc anyone?
    tuple panel_children = allocate_tuple();        
    tuple panel = timm("kind", "g", "children", panel_children);
    
    // sad
    value i;
    tuple node = root;
    vector_foreach(path, i) {
        if (!(node = get(node, intern(i)))) {
            rprintf("bad path: %v\n", path);
        }
    }
    
    u64 *offset = allocate(transient, sizeof(u64));
    *offset = 0;

    if (vector_length(path) > 0) {
        tuple p = allocate_tuple();
        for (int i = 0;i<(vector_length(path) - 1); i++)
            tuple_vector_push(p, vector_get(path, i));
        
        string back = allocate_string();
        bprintf(back, "back");
        add_directory_entry(p, panel_children, offset, back);
    }
    binding_handler an = closure(h, add_node, path, panel_children, offset);
    // incremental key-only
    subkeys(node, an);
    return panel;
    //tuple pt = tuple_from_vector(build_vector(h, sym(children), sym(panel)));
    // absolute?
}

// this is a functional_tuple, but we only care about get here
// maybe not the best implementation strategy for functional maps
// in tuplespace

closure_function(1, 1, value, get_generate_panel, tuple, root, value, v) 
{
    // validate tagof(v) = tuple?
    // really transient?
    return generate_panel(transient, bound(root), v);
}

extern timerheap runloop_timers;

closure_function(1, 1, void, ui_input, session, s, value, v)
{
    session s = bound(s);
    rprintf("ui input %v\n", v);
    // this is a subscription..merge generate in tree properly once
    // we are a little more settled
    table_foreach(v, k1, v3) {
        if (k1 == sym(write)) {
            value wn = table_find(v3, sym(name));
            value wv = table_find(v3, sym(value));

            // shouldn't have interns in the parse path .. fix the
            // lifetime issues
            if (intern(table_find(wn, sym(0))) == sym(generate)) {
                buffer out = allocate_buffer(transient, 100);
                rprintf("wv: %v\n", wv);                
                vector vpath = vector_from_tuple(transient, wv);
                rprintf("vpath: %V\n", vpath);
                tuple k = generate_panel(transient, s->root, vpath);
                tuple w = json_write(timm("0", "ui", "1", "children", "2", "panel"), k);
                format_json(out, w);
                apply(s->out, out);
            }
        }
    }
}
                 
closure_function(1, 1, status, each_ws, session, s, buffer, b)
{
    session s = bound(s);
    if (b) {
        rprintf("from websocket %b\n", b);
        value_handler each_value = closure(s->h, ui_input, s);
        // dont need to allocate this closure all the time
        apply(parse_json(s->h, each_value), b);
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
            s->h = h;
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

    table_set(root, sym(generate), closure(h, get_generate_panel, root));
        
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
