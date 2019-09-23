// Copyright 2012 Rui Ueyama. Released under the MIT license.

/*
 * Tokenizer
 *
 * This is a translation phase after the phase 1 and 2 in file.c.
 * In this phase, the source code is decomposed into preprocessing tokens.
 *
 * Each comment is treated as if it were a space character.
 * Space characters are removed, but the presence of the characters is
 * recorded to the token that immediately follows the spaces as a boolean flag.
 * Newlines are converted to newline tokens.
 *
 * Note that the pp-token is different from the regular token.
 * A keyword, such as "if", is just an identifier at this stage.
 * The definition of the pp-token is usually more relaxed than
 * the regular one. For example, ".32e." is a valid pp-number.
 * Pp-tokens are converted to regular tokens by the C preprocesor
 * (and invalid tokens are rejected by that).
 * Some tokens are removed by the preprocessor (e.g. newline).
 * For more information about pp-tokens, see C11 6.4 "Lexical Elements".
 */

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "8cc.h"

static Token *eof_token;

SourceLoc *get_pos(int offset);

static buffer pos_string(heap h, SourceLoc *s) {
    //    File *f = current_file();
    return aprintf(h, "%d:%d", s->line, s->column);
}

#define errorp(p, ...) 
#define warnp(p, ...)  

static Token *make_token(heap h, SourceLoc *s, Token *tmpl) {
    Token *r = allocate(h, sizeof(Token));
    *r = *tmpl;
    r->s = s;
    return r;
}

static Token *make_ident(heap h, buffer b) {
    return make_token(h, 0, &(Token){ sym(ident), .sval = b });
}

static Token *make_keyword(heap h, SourcePos f, symbol id) {
    return make_token(h, f, &(Token){ sym(keyword), .id = id });
}

static Token *make_number(heap h, SourcePos f, buffer s) {
    return make_token(h, f, &(Token){ sym(number), .sval = s });
}

static Token *make_char(heap h, SourcePos f, int c) {
    return make_token(h, f, &(Token){ sym(char), .c = c});
}

static boolean iswhitespace(int c) {
    return c == ' ' || c == '\t' || c == '\f' || c == '\v';
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

static boolean do_skip_space(buffer b) {
    if (buffer_length(b) == 0) return false;
    int c = readc(b);
    if (iswhitespace(c)) return true;
    if (c == '/') {
        if (next(b, '*')) {
            // SourceLoc *p = get_pos(-2);
            boolean maybe_end = false;
            for (;;) {
                if (buffer_length(b) == 0)
                    errorp(p, "premature end of block comment");
                int c = readc(b);
                if (c == '/' && maybe_end)
                    return true;
                maybe_end = (c == '*');
            }            
            return true;
        }
        if (next(b, '/')) {
            for (;;) {
                if (buffer_length(b) == 0) return true;
                int c = readc(b);
                // keep going
                if (c == '\n') {
                    unreadc(c);
                    return true;
                }
            }            
            return true;
        }
    }
    unreadc(c);
    return false;
}

// Skips spaces including comments.
// Returns true if at least one space is skipped.
static boolean skip_space(buffer b) {
    if (!do_skip_space(b))
        return false;
    while (do_skip_space(b));
    return true;
}

static void skip_char(buffer b) {
    // this isn't right
    if (readc(b) == '\\')
        readc(b);
    char c;
    while ((buffer_length(b) > 0) && (c = readc(b), c != '\''));
}

// Reads a number literal. Lexer's grammar on numbers is not strict.
// Integers and floating point numbers and different base numbers are not distinguished.
static Token *read_number(buffer b) {
    buffer d = allocate_buffer(transient, 10);
    for (;;) {
        int c = readc(b);
        if (isdigit(c) || isalpha(c)) {
            b->start++;
            buffer_write_byte(d, c);                    
        } else {
            return make_number(d);
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
    if (!nextoct())
        return r;
    r = (r << 3) | (readc(b) - '0');
    if (!nextoct())
        return r;
    return (r << 3) | (readc(b) - '0');
}

// Reads a \x escape sequence.
static int read_hex_char(buffer b) {
    SourceLoc *p = get_pos(-2);
    int c = readc(b);
    if (!isxdigit(c))
        errorp(p, "\\x is not followed by a hexadecimal character: %c", c);
    int r = 0;
    for (;; c = readc(b)) {
        switch (c) {
        case '0' ... '9': r = (r << 4) | (c - '0'); continue;
        case 'a' ... 'f': r = (r << 4) | (c - 'a' + 10); continue;
        case 'A' ... 'F': r = (r << 4) | (c - 'A' + 10); continue;
        default: unreadc(c); return r;
        }
    }
}

static int read_escaped_char(buffer b) {
    SourceLoc *p = get_pos(-1);
    int c = readc(b);
    // This switch-cases is an interesting example of magical aspects
    // of self-hosting compilers. Here, we teach the compiler about
    // escaped sequences using escaped sequences themselves.
    // This is a tautology. The information about their real character
    // codes is not present in the source code but propagated from
    // a compiler compiling the source code.
    // See "Reflections on Trusting Trust" by Ken Thompson for more info.
    // http://cm.bell-labs.com/who/ken/trust.html
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
    c = readc();
    if (c != '\'')
        errorp(pos, "unterminated char");
    return make_char(transient, b, r);
}

// Reads a string literal.
static Token *read_string(heap h, buffer b) {
    buffer b = allocate_buffer(h, 10);
    for (;;) {
        if (buffer_length(b) == 0)  errorp(pos, "unterminated string");
        int c = readc(b);
        if (c == '"')
            break;
        if (c != '\\') {
            buffer_write_byte(b, c);
            continue;
        }
        c = read_escaped_char();
        buffer_write_byte(b, c);        
    }
    return make_token(&(Token){ sym(string), .sval = b});
}

static Token *read_ident(heap h, char c) {
    buffer b = allocate_buffer(h, 10);
    buffer_write_byte(b, c);
    for (;;) {
        u8 c = *(u8 *)buffer_ref(b, 0);
        // check to make sure this handles utf8
        if (isalnum(c) || (c & 0x80) || c == '_' || c == '$') {
            buffer_write_byte(b, c);
            b->start++;
            continue;
        }
        return make_ident(h, b);
    }
}

// Reads a digraph starting with '%'. Digraphs are alternative spellings
// for some punctuation characters. They are useless in ASCII.
// We implement this just for the standard compliance.
// See C11 6.4.6p3 for the spec.
static Token *read_hash_digraph(buffer b) {
    if (next(b, '>'))
        return make_keyword('}');
    if (next(b, ':')) {
        if (next(b, '%')) {
            if (next(b, ':'))
                return make_keyword(KHASHHASH);
            unreadc('%');
        }
        return make_keyword('#');
    }
    return NULL;
}

static Token *read_rep(char expect, int t1, int els) {
    return make_keyword(next(b, expect) ? t1 : els);
}

static Token *read_rep2(char expect1, int t1, char expect2, int t2, char els) {
    if (next(b, expect1))
        return make_keyword(t1);
    return make_keyword(next(b, expect2) ? t2 : els);
}

// not the prettiest state machine at the ball
static Token *do_read_token(buffer b) {
    if (buffer_length(b)  == 0) {
        return eof_token;
    }
    u8 c = *(u8 *)buffer_ref(b, 0);
    switch (c) {
    case '\n': return newline_token;
    case ':': return make_keyword(next(b, '>') ? ']' : ':');
    case '#': return make_keyword(next(b, '#') ? KHASHHASH : '#');
    case '+': return read_rep2('+', OP_INC, '=', OP_A_ADD, '+');
    case '*': return read_rep('=', OP_A_MUL, '*');
    case '=': return read_rep('=', OP_EQ, '=');
    case '!': return read_rep('=', OP_NE, '!');
    case '&': return read_rep2('&', OP_LOGAND, '=', OP_A_AND, '&');
    case '|': return read_rep2('|', OP_LOGOR, '=', OP_A_OR, '|');
    case '^': return read_rep('=', OP_A_XOR, '^');
    case '"': return read_string(ENC_NONE);
    case '\'': return read_char(ENC_NONE);
    case '/': return make_keyword(next(b, '=') ? OP_A_DIV : '/');
    case 'a' ... 't': case 'v' ... 'z': case 'A' ... 'K':
    case 'M' ... 'T': case 'V' ... 'Z': case '_': case '$':
    case 0x80 ... 0xFD:
        return read_ident(b);
    case '0' ... '9':
        return read_number(b);
    case '.':
        if (isdigit(peek()))
            return read_number(c);
        if (next(b, '.')) {
            if (next(b, '.'))
                return make_keyword(KELLIPSIS);
            return make_ident("..");
        }
        return make_keyword('.');
    case '(': case ')': case ',': case ';': case '[': case ']': case '{':
    case '}': case '?': case '~':
        return make_keyword(c);
    case '-':
        if (next(b, '-')) return make_keyword(OP_DEC);
        if (next(b, '>')) return make_keyword(OP_ARROW);
        if (next(b, '=')) return make_keyword(OP_A_SUB);
        return make_keyword('-');
    case '<':
        if (next(b, '<')) return read_rep('=', OP_A_SAL, OP_SAL);
        if (next(b, '=')) return make_keyword(OP_LE);
        if (next(b, ':')) return make_keyword('[');
        if (next(b, '%')) return make_keyword('{');
        return make_keyword('<');
    case '>':
        if (next(b, '=')) return make_keyword(OP_GE);
        if (next(b, '>')) return read_rep('=', OP_A_SAR, OP_SAR);
        return make_keyword('>');
    case '%': {
        Token *tok = read_hash_digraph();
        if (tok)
            return tok;
        return read_rep('=', OP_A_MOD, '%');
    }
    }
}

boolean is_keyword(Token *tok, int c) {
    return (tok->kind == sym(keyword)) && (tok->id == c);
}


void init_token()
{
    eof_token= &(Token){ sym(eof) };
}
 
