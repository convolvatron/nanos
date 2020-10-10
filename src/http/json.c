#include <runtime.h>
#include <http/http.h>

typedef struct json_parser *json_parser;
typedef void *(*jparser)(json_parser, character);
typedef jparser (*completion)(json_parser);

#define numeric(__p, __c, __start, __end, __offset, __base)\
    (((__c <= __end) && (__c >= __start))?                                \
     (__p->n = __p->n * __base + (__c - __start + __offset), true):false)


#define sstring(__x) ({ \
     static int init = 0;\
     /* must mark immutable?                    */\
     static struct buffer b;\
     if (!init) {\
      b.start = 0;\
      b.end = sizeof(__x) -1;\
      b.contents = __x;\
      b.length = b.end;\
      init = 1;\
     }        \
     &b;\
    })

void escape_json(buffer out, string current)
{
    buffer_write_byte(out , '"');
    string_foreach(ch, current) {
        if(ch == '\\' || ch == '"') {
            bprintf(out , "\\");
        } else if(ch == '\n') {
            bprintf(out , "\\n");
            continue;
        } else if(ch == '\t') {
            bprintf(out , "\\t");
            continue;
        }
        buffer_write_byte(out , ch);
    }
    buffer_write_byte(out , '"');
}

typedef u64 number; // bear with

struct json_parser {
    heap h;
    value_handler out;

    jparser p;
    buffer string_result;
    number n;
    buffer check;

    // well, we replaced the comparatively expension continuation stack with
    // all these different stacks...they could be unified, but meh
    vector completions;
    vector indices;
    vector tags;

    status_handler error;
};

static void *parse_value(json_parser p, character c);

static inline boolean whitespace(character x)
{
    if ((x == 0x20) ||
        (x == 0x09) ||
        (x == 0x0a) ||
        (x == 0x0d)) return true;
    return false;
}

static char escape_map[] = {
    0x22,  0x22, // " quotation mark
    0x5C,  0x5c, // \ reverse solidus
    0x2F,  0x2f, // / solidus
    0x62,  0x08, // b, backspace
    0x66,  0x0c, // f, form feed
    0x6E,  0x0a, // n, line feed
    0x72,  0x0d, // r, carriage retur
    0x74,  0x09, // t, tab
};

 static char real_escape_map_storage[0x80];
 static char *real_escape_map = 0;


#define complete(__p)  ((completion)vector_pop(__p->completions))(__p)
#define error(__text) return ((void *)0);

static void *parse_string(json_parser p, character c);


static void *parse_decimal_number(json_parser p, character c)
{
    if (numeric(p, c, '0', '9', 0, 10)) return parse_decimal_number;
    return complete(p)(p, c);
}

typedef u64 number;

value box_number(number n)
{
    return (void *)0;
}

//
// float
//
static void *finish_float(json_parser p)
{
    vector_push(p->tags, box_number(p->n));
    return complete(p);
}

static void *negate(json_parser p)
{
    vector_push(p->tags, box_number(-p->n));
    return complete(p);
}

static void *exponent_complete(json_parser p)
{
    //    p->float_result = p->float_result * pow(10.0, (double)(p->number));
    return complete(p);
}

//  xxx presumably the exponent can be negative
static void *check_exp(json_parser p, character c)
{
    if ((c == 'e') || (c == 'E')) {
        vector_push(p->completions, exponent_complete);
        p->n = 0;
        return parse_decimal_number;
    }
    return complete(p)(p, c);
}

static void *parse_fractional(json_parser p, character c)
{
    if ((c >= '0') && (c <= '9')) {
        //   p->n *= 10 + (c - '0');
        return parse_fractional;
    }
    return check_exp(p, c);
}

static void *start_parse_float(json_parser p, character c)
{
    if (numeric(p, c, '0', '9', 0, 10)) return start_parse_float;
    //    p->float_result = (double)p->n;
    if (c == '.') {
        p->n = 10;
        return parse_fractional;
    }
    return check_exp(p, c);
}

static void *parse_hex_number(json_parser p,  character c)
{
    if (numeric(p, c, '0', '9', 0, 10)) return parse_hex_number;
    if (numeric(p, c, 'a', 'f', 10, 16)) return parse_hex_number;
    if (numeric(p, c, 'A', 'F', 10, 16)) return parse_hex_number;
    return complete(p);
}

//
// strings
//
// some crap about 'surrogate pair' for unicode values greater than 16 bits
static void *unicode_complete(buffer b, void *x)
{
    push_character(b, *(u64 *)x);
    return parse_string;
}

static void *parse_backslash(json_parser p, character c)
{
    if (c == 'u') {
        vector_push(p->completions, unicode_complete);
        // xxx - really this is supposed to be exactly 4 digits
        return parse_hex_number;
    }
    character trans = c;
    if (c < sizeof(real_escape_map_storage)) {
        if (!(trans = real_escape_map[c]))
            trans = c;
    }
    push_character(p->string_result, trans);
    return parse_string;
}

static void *parse_string(json_parser p, character c)
{
    if (c == '\\') return parse_backslash;
    if (c == '"')  {
        // some of these should be symbols?
        string s = allocate_string();
        push_buffer(s, p->string_result);
        vector_push(p->tags, s);
        return complete(p);
    }
    push_character(p->string_result, c);
    return parse_string;
}

//
// arrays
//
static void *complete_array(json_parser p)
{
    // single stack
    vector_pop(p->indices);
    return complete(p);
}

static jparser value_complete_array(json_parser p);
static void *next_array(json_parser p, character c)
{
    switch(c) {
    case ',':
        vector_push(p->completions, value_complete_array);
        return parse_value;
    case ']':
        return complete_array(p);
    default:
        if (whitespace(c)) return next_array;
        error("unexpected character at");
    }
}

static jparser value_complete_array(json_parser p)
{
    u64 count = (u64)vector_pop(p->indices);
    value v = vector_pop(p->tags);    
    table_set(vector_peek(p->tags), box_number(count), v);
    count++;
    vector_push(p->indices, (void *)count);
    return next_array;
}

// unfortunately zero is a special case
static void *first_array_element(json_parser p, character c)
{
    if(c == ']') return complete_array(p);
    vector_push(p->completions, value_complete_array);
    return(parse_value(p, c));
}

static void *start_array(json_parser p)
{
    vector_push(p->tags, allocate_tuple());
    vector_push(p->indices, (void *)0);
    return first_array_element;
}

//
// objects
//
static void *next_object(json_parser p, character c);

static void *value_complete_object(json_parser p)
{

    value v = vector_pop(p->tags);
    value k = vector_pop(p->tags);
    table_set(vector_peek(p->tags), intern(k), v);
    return next_object;
}

static void *check_sep(json_parser p, character c)
{
    if (whitespace(c)) return check_sep;
    if (c == ':') {
        vector_push(p->completions, value_complete_object);
        return parse_value;
    }
    error("expected separator");
}

static void *complete_tag(json_parser p)
{
    return check_sep;
}

static void *parse_attribute(json_parser p, character c)
{
    switch(c) {
    case '"':
        vector_push(p->completions, complete_tag);
        buffer_clear(p->string_result);
        return parse_string;
    // xxx - this allows ",}"
    case '}':
        return complete(p);
    default:
        if (whitespace(c)) return parse_attribute;
        error("i was looking for a tag, what did i find?");
    }
}

static void *next_object(json_parser p, character c)
{
    switch(c) {
    case ',':
        return parse_attribute;
    case '}':
        return complete(p);
    default:
        if (whitespace(c)) return next_object;
        error("unexpected character at");
    }
}

static void *start_object(json_parser p)
{
    vector_push(p->tags, allocate_tuple());
    return parse_attribute;
}

//
// immediates
//
static void *parse_immediate(json_parser p, character c)
{
    if (c == *(unsigned char *)buffer_ref(p->check, p->n)) {
        p->n++;
        if (p->n == buffer_length(p->check))
            return complete(p);
        return parse_immediate;
    }
    error("syntax error");
}

static void *start_immediate(json_parser p, buffer b, value v)
{
    p->check = b;
    p->n = 1;
    vector_push(p->tags, v); // ? 
    return  parse_immediate;
}

static void *parse_value(json_parser p, character c)
{
    if (((c >= '0') && (c <= '9')) || (c == '-')) {
        p->n = 0;
        vector_push(p->completions, finish_float);
        if (c == '-') {
            vector_push(p->completions, negate);
            return start_parse_float;
        } else return start_parse_float(p, c);
    }

    switch(c) {
    case '{': return start_object(p);
    case '[': return start_array(p);
    case '"':
        buffer_clear(p->string_result);
        return parse_string;
    case 'f': return start_immediate(p, sstring("false"), (void *)false);
    case 't': return start_immediate(p, sstring("true"), (void *)true);
    case 'n': return start_immediate(p, sstring("null"), 0);
    default:
        if (whitespace(c)) return parse_value;
    }
    error("syntax error looking for value at");
}

static void *json_top(json_parser p, character c);
static jparser top_complete(json_parser p)
{
    // no flow control
    apply(p->out, vector_pop(p->tags));
    vector_push(p->completions, top_complete);
    return json_top;
}

static void *json_top(json_parser p, character c)
{
    switch(c) {
    case '{':
        return start_object(p);
    case '[':
        return start_array(p);
    default:
        if (whitespace(c)) return json_top;
        error("syntax error looking for value at");
    }
}

closure_function(1, 1, status, json_input,
                 json_parser, p,
                 buffer, b)
{
    json_parser p = bound(p);
    if (!b) {
        if (vector_length(p->tags))
            apply(p->error, timm("error", aprintf(p->h, "unterminated json")));
        apply(bound(p)->out, 0);
        return STATUS_OK;
    }
    while(1) {
        int len, blen = buffer_length(b);
        if (!blen) {
            return STATUS_OK;
        }

        character c = utf8_decode(buffer_ref(b, 0), &len);
        if (len <= blen) {
            p->p = p->p(p, c);
            if (!p->p) rprintf("error: %c\n", c);
            b->start += len;
        } else rprintf("oh man, framing boundary split a utf8, what am i ever going to do? %d %d\n", len, blen);
    }
}

buffer_handler parse_json(heap h, value_handler j)
{
    if (!real_escape_map) {
        real_escape_map = real_escape_map_storage;
        for (int i = 0; i < sizeof(escape_map); i+= 2)
            real_escape_map[escape_map[i]] = escape_map[i+1];
    }
    json_parser p= allocate(h, sizeof(struct json_parser));
    p->h = h;
    p->completions = allocate_vector(p->h, 10);
    p->indices = allocate_vector(p->h, 10);
    p->tags = allocate_vector(p->h, 10);
    p->out = j;
    p->string_result = allocate_buffer(p->h, 20);
    vector_push(p->completions, top_complete);
    p->p = json_top;
    return closure(p->h, json_input, p);
}

void format_json(buffer b, value v)
{
    // not going to attempt to reconstruct arrays today
    switch (tagof(v)) {
    case tag_tuple:
        {
            boolean first = true;
            bprintf(b, "{");
            table_foreach((table)v, k, v2) {
                if (!first) {
                    bprintf(b, ",");
                }
                first = false;
                format_json(b, k);
                bprintf(b,": ");
                format_json(b, v2);                
            }
            bprintf(b, "}");
        }
        break;
        
    case tag_symbol:
        bprintf(b, "\"%b\"", symbol_string(v));
        break;        
    case tag_string:
        bprintf(b, "\"%b\"", v);        
        break;
    default:
        halt ("bad json value %d", tagof(v));
    }
}
