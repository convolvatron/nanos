// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include "pacc.h"

location get_pos(int offset);
#define errorp(p, ...) 
#define warnp(p, ...)  

static Token *make_token(heap h, location s, Token *tmpl) {
    Token *r = allocate(h, sizeof(Token));
    *r = *tmpl;
    r->s = s;
    return r;
}

static Token *make_ident(heap h, buffer b) {
    return make_token(h, 0, &(Token){ sym(ident), .sval = b });
}

static Token *make_keyword(heap h, location f, symbol id) {
    return make_token(h, f, &(Token){ sym(keyword), .id = id });
}

static Token *make_number(heap h, location f, buffer s) {
    return make_token(h, f, &(Token){ sym(number), .sval = s });
}

static Token *make_char(heap h, location f, int c) {
    return make_token(h, f, &(Token){ sym(char), .c = c});
}

static char readc(buffer b)
{
    return *(u8 *)buffer_ref(b, 0);    
}


static boolean next(buffer b, int expect) {
    u8 c = readc(b);
    if (c == expect){
        b->start++;
        return true;
    }
    return false;
}

// Reads a number literal. Lexer's grammar on numbers is not strict.
// Integers and floating point numbers and different base numbers are not distinguished.
static Token *read_number(buffer b) {
    buffer d = allocate_buffer(transient, 10);
    for (;;) {
        int c = readc(b);
        // this actually checks for hex, but oddly is safe in this case
        if (digit_of(c) > 0 || isalpha(c)) {
            b->start++;
            buffer_write_byte(d, c);                    
        } else {
            return make_number(transient, 0, d);
        }
    }
}

static boolean nextoct(buffer b) {
    int c = readc(b);
    return '0' <= c && c <= '7';
}

// Reads an octal escape sequence.
static int read_octal_char(buffer b, int c) {
    int r = c - '0';
    if (!nextoct(b))
        return r;
    r = (r << 3) | (readc(b) - '0');
    if (!nextoct(b))
        return r;
    return (r << 3) | (readc(b) - '0');
}

// Reads a \x escape sequence.
static int read_hex_char(buffer b) {
    //    location p = get_pos(-2);
    int c = readc(b);
    if (digit_of(c)< 0)
        errorp(p, "\\x is not followed by a hexadecimal character: %c", c);
    int r = 0;
    for (;; c = readc(b)) {
        switch (c) {
        case '0' ... '9': r = (r << 4) | (c - '0'); b->start++;continue;
        case 'a' ... 'f': r = (r << 4) | (c - 'a' + 10);b->start++; continue;
        case 'A' ... 'F': r = (r << 4) | (c - 'A' + 10);b->start++; continue;
        default: return r;
        }
    }
}

static int read_escaped_char(buffer b) {
    //    location p = get_pos(-1);
    int c = readc(b);

    switch (c) {
    case '\'': case '"': case '?': case '\\':
        return c;
    case 'a': return '\a';
    case 'b': return '\b';
    case 'f': return '\f';
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\t';
    case 'v': return '\v';
    case 'e': return '\033';  // '\e' is GNU extension
    case 'x': return read_hex_char(b);
    case '0' ... '7': return read_octal_char(b, c);
    }
    warnp(p, "unknown escape character: \\%c", c);
    return c;
}

static Token *read_char(buffer b) {
    int c = readc(b);
    int r = (c == '\\') ? read_escaped_char(b) : c;
    c = readc(b);
    if (c != '\'')
        errorp(pos, "unterminated char");
    return make_char(transient, 0, r);
}

// Reads a string literal.
static Token *read_string(buffer b) {
    heap h = transient;
    buffer d = allocate_buffer(h, 10);
    for (;;) {
        if (buffer_length(b) == 0)  errorp(pos, "unterminated string");
        int c = readc(b);
        if (c == '"')
            break;
        if (c != '\\') {
            buffer_write_byte(d, c);
            continue;
        }
        c = read_escaped_char(b);
        buffer_write_byte(d, c);        
    }
    return make_token(transient, 0, &(Token){ sym(string), .sval = d});
}

static Token *read_ident(heap h, buffer b) {
    buffer d = allocate_buffer(h, 10);
    for (;;) {
        u8 c = *(u8 *)buffer_ref(b, 0);
        // check to make sure this handles utf8
        if ((digit_of(c) > 0) || isalpha(c) || (c & 0x80) || c == '_' || c == '$') {
            buffer_write_byte(d, c);
            b->start++;
            continue;
        }
        return make_ident(h, d);
    }
}

static Token *read_rep(buffer b, char expect, symbol t1, symbol els) {
    return make_keyword(transient, 0, next(b, expect) ? t1 : els);
}

static Token *read_rep2(buffer b, char expect1, symbol t1, char expect2, symbol t2, symbol els) {
    if (next(b, expect1))
        return make_keyword(transient, 0, t1);
    return make_keyword(transient, 0, next(b, expect2) ? t2 : els);
}

// not the prettiest state machine at the ball
static Token *do_read_token(buffer b) {
    if (buffer_length(b)  == 0) 
        return &(Token){sym(eof)};

    u8 c = *(u8 *)buffer_ref(b, 0);
    switch (c) {
    case '\n': return make_token(transient, 0, &(Token){ sym(newline)});
    // why dont these use rep2..stupid alternate spellings
    case ':': return make_keyword(transient, 0, sym(:));
    case '#': return make_keyword(transient, 0, sym(#));
    case '+': return read_rep2(b, '+', sym(inc), '=', sym(+), sym(+=));
    case '*': return read_rep(b, '=', sym(*=), sym(*));
    case '=': return read_rep(b, '=', sym(=), sym(=));
    case '!': return read_rep(b, '=', sym(!=), sym(!));
    case '&': return read_rep2(b, '&', sym(&), '=', sym(&), sym(&=));
    case '|': return read_rep2(b, '|', sym(|), '=', sym(|), sym(|=));
    case '^': return read_rep(b, '=', sym(^), sym(^));
    case '"': return read_string(b);
    case '\'': return read_char(b);
    case '/': return make_keyword(transient, 0, next(b, '=') ? sym(/) : sym(/));
    case 'a' ... 'z': case 'A' ... 'Z': case '_': case '$':
    case 0x80 ... 0xFD:
        // c is unconsumed
        return read_ident(transient, b);
    case '0' ... '9':
        return read_number(b);
    case '.':
        if (digit_of(readc(b)) > 0)
            return read_number(b);
        if (next(b, '.')) {
            if (next(b, '.'))
                return make_keyword(transient, 0, sym(...));
            return make_ident(transient, staticbuffer(".."));
        }
        return make_keyword(transient, 0, sym(.));
    case '(': case ')': case ',': case ';': case '[': case ']': case '{':
    case '}': case '?': case '~':{
        char k[2]={c, 0};
        return make_keyword(transient, 0, sym_this(k));
    }
    case '-':
        if (next(b, '-')) return make_keyword(transient, 0, sym(dec));
        if (next(b, '>')) return make_keyword(transient, 0, sym(->));
        if (next(b, '=')) return make_keyword(transient, 0, sym(-=));
        return make_keyword(transient, 0, sym(-));
    case '<':
        if (next(b, '<')) return read_rep(b, '=', sym(<<=), sym(<<));
        if (next(b, '=')) return make_keyword(transient, 0, sym(<));
        if (next(b, ':')) return make_keyword(transient, 0, symq("["));
        if (next(b, '%')) return make_keyword(transient, 0, symq("{"));
        return make_keyword(transient, 0, sym(<));
    case '>':
        if (next(b, '=')) return make_keyword(transient, 0, sym(>=));
        if (next(b, '>')) return read_rep(b, '=', sym(>>=), sym(>>));
        return make_keyword(transient, 0, sym(>));
    case '%': {
        return read_rep(b, '=', sym(%=), sym(%));
    }
    }
    return 0;
}

boolean is_keyword(Token *tok, symbol c) {
    return (tok->kind == sym(keyword)) && (tok->id == c);
}
 
