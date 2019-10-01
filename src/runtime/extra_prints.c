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
    foreach(t, n, v) {
        if (sub) {
            push_character(b, ' ');
        }
        bprintf(b, "%b:", symbol_string((symbol)n));
        vformat(b, v);
        sub = true;
    }
    bprintf(b, ")");
}

// copied from print_tuple()
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


static void format_value(buffer dest, struct formatter_state *s, vlist *v)
{
    value x = varg(*v, tuple);
    vformat(dest, x);
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

void init_extra_prints()
{
    register_format('v', format_value, 0);
    register_format('X', format_hex_buffer, 0);
    register_format('T', format_timestamp, 0);
    register_format('R', format_range, 0);
    register_format('C', format_csum_buffer, 0);
}
