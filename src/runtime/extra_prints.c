#include <runtime.h>


static char *hex_digit="0123456789abcdef";
void print_byte(buffer s, u8 f)
{
    push_u8(s, hex_digit[f >> 4]);
    push_u8(s, hex_digit[f & 15]);
}

void print_hex_buffer(buffer s, buffer b)
{
    int len = buffer_length(b);
    int wlen = 4;
    int rowlen = wlen * 4;
    boolean first = true;

    for (int i = 0 ; i<len ; i+= 1) {
        if (!(i % rowlen)) {
            if (!first) push_u8(s, '\n');
            first = false;
            print_byte(s, i>>24);
            print_byte(s, i>>16);
            print_byte(s, i>>8);
            print_byte(s, i);
            push_u8(s, ':');
        }
        if (!(i % wlen)) push_u8 (s, ' ');
        print_byte(s, *(u8 *)buffer_ref(b, i));
        push_u8(s, ' ');
    }
    // better handling of empty buffer
    push_u8(s, '\n');
}

void print_uuid(buffer b, u8 *uuid)
{
    /* UUID format: 00112233-4455-6677-8899-aabbccddeeff */
    for (int i = 0; i < 4; i++)
        bprintf(b, "%02x", uuid[i]);
    bprintf(b, "-%02x%02x-%02x%02x-%02x%02x-", uuid[4], uuid[5], uuid[6],
            uuid[7], uuid[8], uuid[9]);
    for (int i = 10; i < 16; i++)
        bprintf(b, "%02x", uuid[i]);
}

/* just a little tool for debugging */
void print_csum_buffer(buffer s, buffer b)
{
    u64 csum = 0;
    for (int i = 0; i < buffer_length(b); i++)
        csum += *(u8*)buffer_ref(b, i);
    bprintf(s, "%lx", csum);
}

void print_tuple(buffer b, tuple z)
{
    table t = valueof(z);
    boolean sub = false;
    bprintf(b, "(");
    table_foreach(t, n, v) {
        if (sub) {
            push_character(b, ' ');
        }
        bprintf(b, "%b:", symbol_string((symbol)n));
        // xxx print value
        if (tagof(v) == tag_tuple) {
            print_tuple(b, v);
        } else {
            bprintf(b, "%b", v);
        }
        sub = true;
    }
    bprintf(b, ")");
}

// copied from print_tuple()
// xxx -- why?
static void _print_root(buffer b, tuple z, int indent, boolean is_children)
{
    table t = valueof(z);
    boolean sub = false;
    bprintf(b, "(");
    if (is_children)
        push_character(b, '\n');
    table_foreach(t, n, v) {
        if (sub) {
            if (is_children)
                push_character(b, '\n');
            else
                push_character(b, ' ');
        }
        bprintf(b, "%n%b:", is_children ? indent : 0, symbol_string((symbol)n));
        if (n != sym_this(".") && n != sym_this("..") && n != sym(special)) {
            if (tagof(v) == tag_tuple) {
                boolean is_children = n == sym(children);
                _print_root(b, v, is_children ? indent + 4 : indent, is_children);
            } else {
                bprintf(b, "%b", v);
            }
        }
        sub = true;
    }
    if (is_children && indent >= 4)
        bprintf(b, "\n%n", indent - 4);
    bprintf(b, ")");
}

void print_root(buffer b, tuple z)
{
    _print_root(b, z, 0, true);
}

static void format_tuple(buffer dest, struct formatter_state *s, vlist *v)
{
    print_tuple(dest, varg(*v, tuple));
}

static void format_value(buffer dest, struct formatter_state *s, vlist *v)
{
    buffer b;
    value x = varg(*v, value);
    if (!x) {
        bprintf(dest, "(none)");
        return;
    }

    switch(tagof(x)) {
    case tag_tuple:
        print_tuple(dest, (tuple)x);
        break;
    case tag_symbol:
        bprintf(dest, "%b", symbol_string((symbol)x));
        break;
    default:
        b = (buffer)x;
        if (buffer_length(b) > 20)
            bprintf(dest, "{buffer %d}", buffer_length(b));
        else
            bprintf(dest, "%b", b);
    }
}

static void format_hex_buffer(buffer dest, struct formatter_state *s, vlist *a)
{
    buffer b = varg(*a, buffer);
    print_hex_buffer(dest, b);
}

static void format_csum_buffer(buffer dest, struct formatter_state *s, vlist *a)
{
    buffer b = varg(*a, buffer);
    print_csum_buffer(dest, b);
}

static void format_timestamp(buffer dest, struct formatter_state *s, vlist *a)
{
    timestamp t = varg(*a, timestamp);
    print_timestamp(dest, t);
}

static void format_range(buffer dest, struct formatter_state *s, vlist *a)
{
    range r = varg(*a, range);
    bprintf(dest, "[0x%lx 0x%lx)", r.start, r.end);
}

static void format_closure(buffer dest, struct formatter_state *s, vlist *a)
{
    // xxx - we can probably do better here?
    void **k = varg(*a, void **);
    struct _closure_common *c = k[1];
    bprintf(dest, "%s", &c->name);
}

static char *tag_names[] =
    {"unknown",
     "symbol",
     "tuple",
     "string",
     "function_tuple",
     "number",
    };

static void format_kind(buffer dest, struct formatter_state *s, vlist *a)
{
    value v = varg(*a, value);
    int tag = tagof(v);
    char *tag_name;
    if (tag < tag_max) {
        tag_name = tag_names[tag];
    } else {
        tag_name = "invalid";
    }
    bprintf(dest, "%s", tag_name);
}

static void format_keysof(buffer dest, struct formatter_state *s, vlist *a)
{
    boolean first = true;
    value t = varg(*a, tuple);    
    table_foreach(t, n, _) {
        if (!first) buffer_write_byte(dest, ' ');
        bprintf(dest, "%v", n);
        first = false;
    }
}

static void format_value_vector(buffer dest, struct formatter_state *s, vlist *a)
{
    vector v = varg(*a, vector);
    bprintf(dest, "[");
    value i;
    boolean first = true;
    vector_foreach(v, i){
        if (!first) bprintf(dest, " ");
        first = false;
        bprintf(dest, "%v", i);
    }
    bprintf(dest, "]");    
}

void init_extra_prints()
{
    register_format('t', format_tuple, 0);
    register_format('v', format_value, 0);
    register_format('V', format_value_vector, 0);    
    register_format('X', format_hex_buffer, 0);
    register_format('T', format_timestamp, 0);
    register_format('R', format_range, 0);
    register_format('C', format_csum_buffer, 0);
    register_format('F', format_closure, 0);
    register_format('k', format_kind, 0);    // realy type/tag but 't' is pretty booked
    register_format('K', format_keysof, 0);    
}
