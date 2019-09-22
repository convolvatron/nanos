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

static vector buffers = &EMPTY_VECTOR;
static Token *eof_token = &(Token){ TEOF };

static char *pos_string(Pos *p) {
    //    File *f = current_file();
    return format("%s:%d:%d", f ? f->name : "(unknown)", p->line, p->column);
}

#define errorp(p, ...) 
#define warnp(p, ...)  

static Token *make_token(heap h, buffer f, Token *tmpl) {
    Token *r = allocate(h, sizeof(Token));
    *r = *tmpl;
    r->file = f;
    r->line = pos.line;
    r->column = pos.column;
    return r;
}

static Token *make_ident(heap h, buffer f, buffer b) {
    return make_token(h, f, &(Token){ TIDENT, .sval = b });
}

static Token *make_keyword(heap h, buffer f, int id) {
    return make_token(h, f, &(Token){ TKEYWORD, .id = id });
}

static Token *make_number(heap h, buffer f, buffer s) {
    return make_token(h, f, &(Token){ TNUMBER, .sval = s });
}

static Token *make_invalid(heap h, buffer f, char c) {
    return make_token(h, f, &(Token){ TINVALID, .c = c });
}

static Token *make_char(heap h, buffer f, int c) {
    return make_token(h, f, &(Token){ TCHAR, .c = c});
}

static bool iswhitespace(int c) {
    return c == ' ' || c == '\t' || c == '\f' || c == '\v';
}

static bool next(buffer b, int expect) {
    u8 c = *(u8 *)buffer_ref(b, 0);    
    if (c == expect){
        b->start++;
        return true;
    }
    return false;
}

static boolean do_skip_space(buffer b) {
    int c = readc();
    if (c == EOF)
        return false;
    if (iswhitespace(c))
        return true;
    if (c == '/') {
        if (next(b, '*')) {
            Pos p = get_pos(-2);
            bool maybe_end = false;
            for (;;) {
                int c = readc();
                if (c == EOF)
                    errorp(p, "premature end of block comment");
                if (c == '/' && maybe_end)
                    return;
                maybe_end = (c == '*');
            }            
            return true;
        }
        if (next(b, '/')) {
            for (;;) {
                int c = readc();
                if (c == EOF)
                    return;
                if (c == '\n') {
                    unreadc(c);
                    return;
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
static boolean skip_space(b) {
    if (!do_skip_space())
        return false;
    while (do_skip_space());
    return true;
}

static void skip_char(b) {
    if (readc() == '\\')
        readc();
    int c = readc();
    while (c != EOF && c != '\'')
        c = readc();
}

// Reads a number literal. Lexer's grammar on numbers is not strict.
// Integers and floating point numbers and different base numbers are not distinguished.
static Token *read_number(buffer b) {
    buffer b = make_buffer();
    buf_write(b, c);
    char last = c;
    for (;;) {
        int c = readc();
        if (!isdigit(c) && !isalpha(c))
            unreadc(c);
            buf_write(b, '\0');
            return make_number(buf_body(b));
        }
        buf_write(b, c);
        last = c;
    }
}

static bool nextoct() {
    int c = peek();
    return '0' <= c && c <= '7';
}

// Reads an octal escape sequence.
static int read_octal_char(int c) {
    int r = c - '0';
    if (!nextoct())
        return r;
    r = (r << 3) | (readc() - '0');
    if (!nextoct())
        return r;
    return (r << 3) | (readc() - '0');
}

// Reads a \x escape sequence.
static int read_hex_char() {
    Pos p = get_pos(-2);
    int c = readc();
    if (!isxdigit(c))
        errorp(p, "\\x is not followed by a hexadecimal character: %c", c);
    int r = 0;
    for (;; c = readc()) {
        switch (c) {
        case '0' ... '9': r = (r << 4) | (c - '0'); continue;
        case 'a' ... 'f': r = (r << 4) | (c - 'a' + 10); continue;
        case 'A' ... 'F': r = (r << 4) | (c - 'A' + 10); continue;
        default: unreadc(c); return r;
        }
    }
}

static int read_escaped_char() {
    Pos p = get_pos(-1);
    int c = readc();
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
    case 'x': return read_hex_char();
    case 'u': return read_universal_char(4);
    case 'U': return read_universal_char(8);
    case '0' ... '7': return read_octal_char(c);
    }
    warnp(p, "unknown escape character: \\%c", c);
    return c;
}

static Token *read_char(int enc) {
    int c = readc();
    int r = (c == '\\') ? read_escaped_char() : c;
    c = readc();
    if (c != '\'')
        errorp(pos, "unterminated char");
    if (enc == ENC_NONE)
        return make_char((char)r, enc);
    return make_char(r, enc);
}

// Reads a string literal.
static Token *read_string(heap h, buffer b) {
    buffer b = allocate_buffer(h, 10);
    for (;;) {
        int c = readc();
        if (c == EOF)
            errorp(pos, "unterminated string");
        if (c == '"')
            break;
        if (c != '\\') {
            buf_write(b, c);
            continue;
        }
        bool isucs = (peek() == 'u' || peek() == 'U');
        c = read_escaped_char();
        if (isucs) {
            write_utf8(b, c);
            continue;
        }
        buf_write(b, c);
    }
    return make_token(&(Token){ TSTRING, .sval = b})
}

static Token *read_ident(char c) {
    buffer b = make_buffer();
    buffer_write_byte(b, c);
    for (;;) {
        u8 c = *(u8 *)buffer_ref(b, 0);
        // check to make sure this handles utf8
        if (isalnum(c) || (c & 0x80) || c == '_' || c == '$') {
            buffer_write_byte(b, c);
            b->start++;
            continue;
        }
        return make_ident(b);
    }
}

static void skip_block_comment() {

}

// Reads a digraph starting with '%'. Digraphs are alternative spellings
// for some punctuation characters. They are useless in ASCII.
// We implement this just for the standard compliance.
// See C11 6.4.6p3 for the spec.
static Token *read_hash_digraph() {
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

// not the prettiest state machine at 
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
    case 'u':
        if (next(b, '"')) return read_string(ENC_CHAR16);
        if (next(b, '\'')) return read_char(ENC_CHAR16);
        // C11 6.4.5: UTF-8 string literal
        if (next(b, '8')) {
            if (next(b, '"'))
                return read_string(ENC_UTF8);
            unreadc('8');
        }
        return read_ident(c);
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

boolean is_keyword(Token *tok, int c) {
    return (tok->kind == TKEYWORD) && (tok->id == c);
}


