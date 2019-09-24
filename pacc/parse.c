// Copyright 2012 Rui Ueyama. Released under the MIT license.
#include "8cc.h"

static symbol close_paren, open_paren, comma, close_brace, open_brace, semicolon,
    open_bracket, close_bracket, colon;


enum {
    DECL_BODY = 1,
    DECL_PARAM,
    DECL_PARAM_TYPEONLY,
    DECL_CAST,
};

#define NULL ((void *)0)

Token *token(parse p)
{
    return 0;
}

void consume(parse p)
{
}

string make_tempname() {
    static int c = 0;
    return aprintf(transient, ".T%d", c++);
}

string make_label() {
    static int c = 0;
    return aprintf(transient, ".L%d", c++);
}

static string make_static_label(buffer name) {
    static int c = 0;
    return aprintf(transient, ".S%d.%s", c++, name);
}

static Case *make_case(int beg, int end, buffer label) {
    Case *r = allocate(transient, sizeof(Case));
    r->beg = beg;
    r->end = end;
    r->label = label;
    return r;
}

static Node *make_ast(Node *tmpl, location source_loc) {
    Node *r = allocate(transient, sizeof(Node));
    *r = *tmpl;
    r->sourceLoc = source_loc;
    return r;
}

static Node *ast_uop(symbol kind, Type *ty, Node *operand) {
    return make_ast(&(Node){ kind, ty, .operand = operand }, 0);
}

static Node *ast_binop(parse p, Type *ty, symbol kind, Node *left, Node *right) {
    Node *r = make_ast(&(Node){ kind, ty }, 0);
    r->left = left;
    r->right = right;
    return r;
}

static Node *ast_inttype(Type *ty, long val) {
    return make_ast(&(Node){ sym(literal), ty, .ival = val }, 0);
}

// aren't these the same(local and global)?
static Node *ast_lvar(parse p, Type *ty, buffer name) {
    Node *r = make_ast(&(Node){ sym(lvar), ty, .varname = name }, 0);
    if (p->localenv)
        set(p->localenv, name, r);
    if (p->localvars)
        vector_push(p->localvars, r);
    return r;
}

static Node *ast_gvar(parse p, Type *ty, buffer name) {
    Node *r = make_ast(&(Node){ sym(gvar), ty, .varname = name, .glabel = name }, 0);
    set(p->globalenv, name, r);
    return r;
}

static Node *ast_static_lvar(parse p, Type *ty, buffer name) {
    Node *r = make_ast(&(Node){
            .kind = sym(gvar),
                .ty = ty,
                .varname = name,
                .glabel = make_static_label(name) }, 0);
    set(p->localenv, name, r);
    return r;
}

static Type *make_type(Type *tmpl) {
    Type *r = allocate(transient, sizeof(Type));
    *r = *tmpl;
    return r;
}

static Type* make_array_type(Type *ty, int len) {
    int size;
    if (len < 0)
        size = -1;
    else
        size = ty->size * len;
    return make_type(&(Type){
            sym(array),
                .ptr = ty,
                .size = size,
                .len = len,
                .align = ty->align });
}

static Node *ast_string(parse p, buffer in)
{
    Type *ty;
    buffer b = in;
    ty = make_array_type(p->type_char, buffer_length(in));
    return make_ast(&(Node){ sym(literal), .ty = ty, .sval = b }, 0);
}

static Node *ast_funcall(Type *ftype, buffer fname, vector args) {
    return make_ast(&(Node){
            .kind = sym(funcall),
                .ty = ftype->rettype,
                .fname = fname,
                .args = args,
                .ftype = ftype }, 0);
}

static Node *ast_funcdesg(Type *ty, string fname) {
    return make_ast(&(Node){ sym(funcdesg), ty, .fname = fname }, 0);
}

static Node *ast_funcptr_call(Node *fptr, vector args) {
    assert(fptr->ty->kind == sym(ptr));
    assert(fptr->ty->ptr->kind == sym(func));
    return make_ast(&(Node){
            .kind = sym(funcptr_call),
                .ty = fptr->ty->ptr->rettype,
                .fptr = fptr,
                .args = args }, 0);
}

static Node *ast_func(Type *ty, string fname, vector params, Node *body, vector localvars) {
    return make_ast(&(Node){
            .kind = sym(func),
                .ty = ty,
                .fname = fname,
                .params = params,
                .localvars = localvars,
                .body = body}, 0);
}

static Node *ast_decl(Node *var, vector init) {
    return make_ast(&(Node){ sym(decl), .declvar = var, .declinit = init }, 0);
}

static Node *ast_init(Node *val, Type *totype, int off) {
    return make_ast(&(Node){ sym(init), .initval = val, .initoff = off, .totype = totype }, 0);
}

static Node *ast_conv(Type *totype, Node *val) {
    return make_ast(&(Node){ sym(conv), totype, .operand = val }, 0);
}

static Node *ast_if(Node *cond, Node *then, Node *els) {
    return make_ast(&(Node){ sym(if), .cond = cond, .then = then, .els = els }, 0);
}

static Node *ast_ternary(Type *ty, Node *cond, Node *then, Node *els) {
    return make_ast(&(Node){ sym(ternary), ty, .cond = cond, .then = then, .els = els }, 0);
}

static Node *ast_return(Node *retval) {
    return make_ast(&(Node){ sym(return), .retval = retval }, 0);
}

static Node *ast_compound_stmt(vector stmts) {
    return make_ast(&(Node){ sym(compound_stmt), .stmts = stmts }, 0);
}

static Node *ast_struct_ref(Type *ty, Node *struc, buffer name) {
    return make_ast(&(Node){ sym(struct_ref), ty, .struc = struc, .field = name }, 0);
}

static Node *ast_goto(buffer label) {
    return make_ast(&(Node){ sym(goto), .label = label }, 0);
}

static Node *ast_jump(buffer label) {
    return make_ast(&(Node){ sym(goto), .label = label, .newlabel = label }, 0);
}

static Node *ast_computed_goto(Node *expr) {
    return make_ast(&(Node){ sym(computed_goto), .operand = expr }, 0);
}

static Node *ast_label(buffer label) {
    return make_ast(&(Node){ sym(label), .label = label }, 0);
}

static Node *ast_dest(buffer label) {
    return make_ast(&(Node){ sym(label), .label = label, .newlabel = label }, 0);
}


static Type* make_ptr_type(Type *ty) {
    return make_type(&(Type){ sym(ptr), .ptr = ty, .size = 8, .align = 8 }, 0);
}

static Node *ast_label_addr(parse p, buffer label) {
    return make_ast(&(Node){ sym(label_addr), make_ptr_type(p->type_void), .label = label }, 0);
}


static Type *copy_type(Type *ty) {
    Type *r = allocate(transient, sizeof(Type));
    runtime_memcpy(r, ty, sizeof(Type));
    return r;
}

// tabularize, memoize
static Type *make_numtype(symbol kind, boolean usig) {
    Type *r = allocate_zero(transient, sizeof(Type));
    r->kind = kind;
    r->usig = usig;
    if (kind == sym(void))         r->size = r->align = 0;
    else if (kind == sym(boolean))    r->size = r->align = 1;
    else if (kind == sym(char))    r->size = r->align = 1;
    else if (kind == sym(short))   r->size = r->align = 2;
    else if (kind == sym(int))     r->size = r->align = 4;
    else if (kind == sym(long))    r->size = r->align = 8;
    else if (kind == sym(llong))   r->size = r->align = 8;
    else error("internal error");
    return r;
}


static Type* make_rectype() {

}

static Type* make_func_type(Type *rettype, vector paramtypes, boolean has_varargs) {
    return make_type(&(Type){
            sym(func),
                .rettype = rettype,
                .params = paramtypes,
                .hasva = has_varargs});
}

static Type *make_stub_type() {
    return make_type(&(Type){ sym(stub) });
}

/*
 * Predicates and kind checking routines
 */

// make a property of the type
boolean is_inttype(Type *ty) {
    if (ty->kind == sym(boolean) ||
        ty->kind == sym(char) ||
        ty->kind == sym(short) ||
        ty->kind == sym(int) ||
        ty->kind == sym(long) ||
        ty->kind == sym(llong))
        return true;
    return false;
}

static boolean is_arithtype(Type *ty) {
    return is_inttype(ty);
}


static void ensure_lvalue(Node *node) {
    if (node->kind == sym(lvar) ||
        node->kind == sym(gvar) ||
        node->kind == sym(deref) ||
        node->kind == sym(struct_ref))
        return;

    error("lvalue expected, but got %s", node2s(node));
}

static void ensure_inttype(Node *node) {
    if (!is_inttype(node->ty))
        error("integer type expected, but got %s", node2s(node));
}

static void ensure_arithtype(Node *node) {
    if (!is_arithtype(node->ty))
        error("arithmetic type expected, but got %s", node2s(node));
}

static void ensure_not_void(Type *ty) {
    if (ty->kind == sym(VOID))
        error("void is not allowed");
}

static void expect(parse p, symbol id) {
    Token *tok = get_token(p->b);
    if (!is_keyword(tok, id))
        errort(tok, "'%c' expected, but got %s", id, string_from_token(transient, tok));
}

static Type *copy_incomplete_type(Type *ty) {
    if (!ty) return 0;
    return (ty->len == -1) ? copy_type(ty) : ty;
}

static Type *get_typedef(parse p, buffer name) {
    Node *node = get(p->types, intern(name));
    return (node && (node->kind == sym(typedef))) ? node->ty : NULL;
}

static boolean is_type(parse p, Token *tok)
{
    if (tok->kind == sym(ident))
        return get_typedef(p, tok->sval)?true:false;
    if (tok->kind != sym(keyword))
        return false;
    // all the standard types were pulled in with redefining op    
}

static boolean next_token(parse p, symbol kind) {
    Token *tok = token(p);
    if (is_keyword(tok, kind)){
        consume(p);
        return true;
    }
    return false;
}

void *make_pair(heap h, void *first, void *second) {
    void **r = allocate(h, sizeof(void *) * 2);
    r[0] = first;
    r[1] = second;
    return r;
}

static Node *conv(Node *node) {
    Type *ty = node->ty;
    if (ty->kind == sym(array))
        // c11 6.3.2.1p3: an array of t is converted to a pointer to t.
        return ast_uop(sym(conv), make_ptr_type(ty->ptr), node);
    if (ty->kind == sym(func))
        // c11 6.3.2.1p4: a function designator is converted to a pointer to the function.
        return ast_uop(sym(addr), make_ptr_type(ty), node);
    if ((ty->kind == sym(short)) ||
        (ty->kind == sym(char)) ||
        (ty->kind == sym(boolean)))
        // c11 6.3.1.1p2: the integer promotions
        return ast_conv(node->p->type_int, node);
    if (ty->kind == sym(int))
        if (ty->bitsize > 0)
            return ast_conv(node->p->type_int, node);
    return node;
}

static boolean same_arith_type(Type *t, Type *u) {
    return t->kind == u->kind && t->usig == u->usig;
}

static Node *wrap(Type *t, Node *node) {
    if (same_arith_type(t, node->ty))
        return node;
    return ast_uop(sym(conv), t, node);
}

// c11 6.3.1.8: usual arithmetic conversions
static Type *usual_arith_conv(parse p, Type *t, Type *u) {
    assert(is_arithtype(t));
    assert(is_arithtype(u));
    // umm .. uh oh
    if (t->kind < u->kind) {
        // Make t the larger type
        Type *tmp = t;
        t = u;
        u = tmp;
    }
    assert(is_inttype(t) && t->size >= p->type_int->size);
    assert(is_inttype(u) && u->size >= p->type_int->size);
    if (t->size > u->size)
        return t;
    assert(t->size == u->size);
    if (t->usig == u->usig)
        return t;
    Type *r = copy_type(t);
    r->usig = true;
    return r;
}

// op property
static boolean valid_pointer_binop(symbol op) {
    if (op == sym(-) ||
        op == sym(<) ||
        op == sym(>) ||
        op == sym(=) ||
        op == sym(!=) ||
        op == sym(<=) ||
        op == sym(>=))
        return true;
    return false;
}

static Node *binop(parse p, symbol op, Node *lhs, Node *rhs) {
    if (lhs->ty->kind == sym(ptr) && rhs->ty->kind == sym(ptr)) {
        if (!valid_pointer_binop(op))
            error("invalid pointer arith");
        // C11 6.5.6.9: Pointer subtractions have type ptrdiff_t.
        if (op == sym(-))
            return ast_binop(p, p->type_long, op, lhs, rhs);
        // C11 6.5.8.6, 6.5.9.3: Pointer comparisons have type int.
        return ast_binop(p, p->type_int, op, lhs, rhs);
    }
    if (lhs->ty->kind == sym(ptr))
        return ast_binop(p, lhs->ty, op, lhs, rhs);
    if (rhs->ty->kind == sym(ptr))
        return ast_binop(p, rhs->ty, op, rhs, lhs);
    assert(is_arithtype(p, lhs->ty));
    assert(is_arithtype(p, rhs->ty));
    Type *r = usual_arith_conv(p, lhs->ty, rhs->ty);
    return ast_binop(p, r, op, wrap(r, lhs), wrap(r, rhs));
}

static void ensure_assignable(Type *totype, Type *fromtype) {
    if ((is_arithtype(totype) || totype->kind == sym(ptr)) &&
        (is_arithtype(fromtype) || fromtype->kind == sym(ptr)))
        return;
    // there was a structural equivalence here - ignore?
    //    if (is_same_struct(totype, fromtype))
    //        return;
    error("incompatible kind: <%s> <%s>", ty2s(totype), ty2s(fromtype));
}


// xxx little state machines
static Type *read_int_suffix(parse p, buffer b){
    if (buffer_length(b) > 0) {
        if (*(u8 *)buffer_ref(b, 0) == 'u') {
            if (buffer_length(b) > 1) {
                if (*(u8 *)buffer_ref(b, 1) == 'l') {
                    if (buffer_length(b) > 2) {
                        if (*(u8 *)buffer_ref(b, 2) == 'l') {
                            return p->type_ullong;
                        }
                    }
                    return p->type_ulong;
                }
            }
            return p->type_uint;
        }
        
        if (*(u8 *)buffer_ref(b, 0) == 'l') {
            if (buffer_length(b) > 1) {
                if (*(u8 *)buffer_ref(b, 1) == 'l') {
                    if (buffer_length(b) > 2) {
                        if (*(u8 *)buffer_ref(b, 2) == 'u') {
                            return p->type_ullong;
                        }
                    }
                    return p->type_ulong;
                }
            }
            return p->type_uint;
        }
    }
    return NULL;
}

static Type *read_abstract_declarator(parse p, Type *basety) {
    return read_declarator(p, NULL, basety, NULL, DECL_CAST);
}

static buffer read_rectype_tag(parse p) {
    Token *tok = token(p);
    if (tok->kind == sym(ident)) {
        consume(p);
        return tok->sval;
    }
    return NULL;
}

static int read_intexpr(parse p) {
    // xxx - we were doing static evaluation here...pass through
}

static Type *read_enum_def(parse p) {
    buffer tag = NULL;
    Token *tok = token(p);

    // Enum is handled as a synonym for int. We only check if the enum
    // is declared.
    if (tok->kind == sym(ident)) {
        tag = tok->sval;
        tok = token(p);
    }
    if (tag) {
        Type *ty = get(p->tags, tag);
        if (ty && ty->kind != sym(enum))
            errort(tok, "declarations of %s does not match", tag);
    }
    if (!is_keyword(tok, sym({))) {
        if (!tag || !get(p->tags, tag))
            errort(tok, "enum tag %s is not defined", tag);
        return p->type_int;
    }
    consume(p);
    if (tag)
        set(p->tags, tag, p->type_enum);

    int val = 0;
    for (;;) {
        tok = token(p);
        if (is_keyword(tok, sym(})))
            break;
        if (tok->kind != sym(ident))
            errort(tok, "identifier expected, but got %s", tok2s(tok));
        buffer name = tok->sval;

        if (next_token(p, sym(=)))
            val = read_intexpr(p);
        Node *constval = ast_inttype(p->type_int, val++);
        // ?
        set(p->globalenv, name, constval);
        if (next_token(p, sym(intern(sstring","))))
            continue;
        if (next_token(p, sym(})))
            break;
        errort(peek(), "',' or '}' expected, but got %s", tok2s(peek()));
    }
    return p->type_int;
}

static Type *read_decl_spec(parse p, symbol *rsclass);

static Type *read_cast_type(parse p) {
    return read_abstract_declarator(p, read_decl_spec(p, NULL));
}

static Node *read_assignment_expr(parse p);

static Node *read_comma_expr(parse p) {
    Node *node = read_assignment_expr(p);
    while (next_token(p, comma)) {
        Node *expr = read_assignment_expr(p);
        node = ast_binop(p, expr->ty, comma, expr);
    }
    return node;
}

static Type *read_typeof(parse p) {
    expect(p, open_paren);
    Type *r = is_type(p, token(p))
        ? read_cast_type(p)
        : read_comma_expr(p)->ty;
    expect(p, close_paren);
    return r;
}

static boolean is_poweroftwo(int x) {
    // If there's only one bit set in x, the value is a power of 2.
    return (x <= 0) ? false : !(x & (x - 1));
}


static int read_alignas(parse p) {
    // C11 6.7.5. Valid form of _Alignof is either _Alignas(type-name) or
    // _Alignas(constant-expression).
    expect(p, open_paren);
    int r = is_type(p, token(p))
        ? read_cast_type(p)->align
        : read_intexpr(p);
    expect(p, close_paren);
    return r;
}

static Type *read_rectype_def(parse p, symbol kind);

static Type *read_decl_spec(parse p, symbol *rsclass) {
    symbol sclass;
    Token *tok = token(p);
    
    if (!is_type(p, tok))
        errort(tok, "type name expected, but got %s", tok2s(tok));

    Type *usertype = NULL;
    enum { kvoid = 1, kboolean, kchar, kint, kfloat, kdouble } kind = 0;
    enum { kshort = 1, klong, kllong } size = 0;
    enum { ksigned = 1, kunsigned } sig = 0;
    int align = -1;

    for (;;) {
        tok = token(p);  // first one consume?
        if (tok->kind == sym(eof))
            error("premature end of input");
        if (kind == 0 && tok->kind == sym(ident) && !usertype) {
            Type *def = get_typedef(p, tok->sval);
            if (def) {
                if (usertype) goto err;
                usertype = def;
                goto errcheck;
            }
        }
        if (tok->kind != sym(keyword)) {
            break;
        }
        consume(p);
        if ((tok->id != sym(const)) &&
            (tok->id != sym(volatile)) &&
            (tok->id != sym(inline)) &&
            (tok->id != sym(noreturn))) {
                // construct metadata
                if (tok->id == sym(typedef))  {
                    if (sclass) goto err;
                    sclass = sym(typedef);
                }
                if (tok->id == sym(extern)) {
                    if (sclass) goto err;
                    sclass = sym(extern);
                }
                if (tok->id == sym(static)){
                    if (sclass) goto err;
                    sclass = sym(static);
                }
                if (tok->id == sym(auto)) {
                    if (sclass) goto err;
                    sclass = sym(auto);
                }
                if (tok->id == sym(register)) {
                    if (sclass) goto err; sclass = sym(register);
                }
                if (tok->id == sym(void))     {if (kind) goto err; kind = kvoid; }
                if (tok->id == sym(boolean))  {if (kind) goto err; kind = kboolean; }
                if (tok->id == sym(char))     {if (kind) goto err; kind = kchar; }
                if (tok->id == sym(int))      {if (kind) goto err; kind = kint; }
                if (tok->id == sym(signed))   {if (sig) goto err; sig = ksigned; }
                if (tok->id == sym(unsigned)) {if (sig) goto err; sig = kunsigned; }
                if (tok->id == sym(short))    {if (size) goto err; size = kshort;}
                
                if ((tok->id == sym(struct)) || (tok->id == sym(union)))
                    {if (usertype) goto err; usertype = read_rectype_def(p, tok->id); }
                if (tok->id == sym(enum))     {if (usertype) goto err; usertype = read_enum_def(p); }
                if (tok->id == sym(alignas)) {
                    int val = read_alignas(p);
                    if (val < 0)
                        errort(tok, "negative alignment: %d", val);
                    // C11 6.7.5p6: alignas(0) should have no effect.
                    if (val != 0) {
                        if (align == -1 || val < align)
                            align = val;
                    }
                }
                if (tok->id == sym(long)) {
                if (size == 0) size = klong;
                else if (size == klong) size = kllong;
                else goto err;
                
            }
                if (tok->id == sym(typeof)) {
                if (usertype) goto err;
                usertype = read_typeof(p);
            }
                goto done;
        }
    errcheck:
        consume(p);
        if (kind == kboolean && (size != 0 && sig != 0))
            goto err;
        if (size == kshort && (kind != 0 && kind != kint))
            goto err;
        if (size == klong && (kind != 0 && kind != kint && kind != kdouble))
            goto err;
        if (sig != 0 && (kind == kvoid || kind == kfloat || kind == kdouble))
            goto err;
        if (usertype && (kind != 0 || size != 0 || sig != 0))
            goto err;
    }
 done:
    if (rsclass)
        *rsclass = sclass;
    if (usertype)
        return usertype;
    if (align != -1 && !is_poweroftwo(align))
        errort(tok, "alignment must be power of 2, but got %d", align);
    Type *ty;
    if (kind == kvoid) {  ty = p->type_void; goto end;}
    if (kind == kboolean) {  ty = make_numtype(sym(boolean), false); goto end;}
    if (kind == kchar){   ty = make_numtype(sym(char), sig == kunsigned); goto end;}
    if (size == kshort) {ty = make_numtype(sym(short), sig == kunsigned); goto end;}
    if (size == klong) {  ty = make_numtype(sym(long), sig == kunsigned); goto end;}
    if (size == kllong){ ty = make_numtype(sym(llong), sig == kunsigned); goto end;}
    {ty = make_numtype(sym(int), sig == kunsigned); goto end;}
    error("internal error: kind: %d, size: %d", kind, size);
 end:
    if (align != -1)
        ty->align = align;
    return ty;
 err:
    errort(tok, "type mismatch: %s", tok2s(tok));
}

static vector read_rectype_fields_sub(parse p);
static vector read_rectype_fields(parse p, int *rsize, int *align);

static Type *read_rectype_def(parse p, symbol kind) {
    buffer tag = read_rectype_tag(p);
    Type *r;
    if (tag) {
        r = get(p->tags, tag);
        if (r && (r->kind == sym(enum) || r->kind != kind))
            error("declarations of %s does not match", tag);
        if (!r) {
            r =  make_type(&(Type){ kind});
            set(p->tags, tag, r);
        }
    } else {
        r =  make_type(&(Type){ kind});            
    }
    int size = 0, align = 1;
    vector fields = read_rectype_fields(p, &size, &align, kind);
    r->align = align;
    if (fields) {
        r->fields = fields;
        r->size = size;
    }
    return r;
}


static vector read_rectype_fields_sub(parse p) {
    vector r = allocate_vector(p->h, 10);
    for (;;) {
        if (!is_type(p, token(p)))
            break;
        Type *basetype = read_decl_spec(p, NULL);
        if (basetype->kind == sym(struct) && next_token(p, sym(;))) {
            vector_push(r, make_pair(p->h, NULL, basetype));
            continue;
        }
        for (;;) {
            buffer name = NULL;
            Type *fieldtype = read_declarator(&name, basetype, NULL, DECL_PARAM_TYPEONLY);
            ensure_not_void(fieldtype);
            fieldtype = copy_type(fieldtype);
            fieldtype->bitsize = next_token(p, colon) ? read_bitsize(name, fieldtype) : -1;
            vector_push(r, make_pair(p->h, name, fieldtype));
            if (next_token(p, comma))
                continue;
            if (is_keyword(token(p), close_brace))
                warnt(peek(), "missing ';' at the end of field list");
            else
                expect(p, semicolon);
            break;
        }
    }
    expect(p, close_brace);
    return r;
}

static vector read_rectype_fields(parse p, int *rsize, int *align) {
    if (!next_token(p, open_brace))
        return NULL;
    vector fields = read_rectype_fields_sub(p);
    //     fix_rectype_flexible_member(fields);
    return fields;
}

static Type *read_sizeof_operand_sub(parse p) {
    Token *tok = get_token(p->b);
    if (is_keyword(tok, intern(staticbuffer("("))) && is_type(p, token(p))) {
        Type *r = read_cast_type(p);
        consume(p);
        expect(p, intern(staticbuffer(")")));
        return r;
    }
    return read_unary_expr(p)->ty;
}

static Node *read_sizeof_operand(parse p) {
    Type *ty = read_sizeof_operand_sub(p);
    // Sizeof on void or function type is GNU extension
    int size = (ty->kind == sym(void) || ty->kind == sym(func)) ? 1 : ty->size;
    assert(0 <= size);
    return ast_inttype(p->type_ulong, size);
}

static Node *read_alignof_operand(parse p) {
    expect(p, open_paren);
    Type *ty = read_cast_type(p);
    expect(p, close_paren);
    return ast_inttype(p->type_ulong, ty->align);
}

static vector read_func_args(parse p,  vector params) {
    vector args = allocate_vector(p->h, 10);
    int i = 0;
    for (;;) {
        if (next_token(p, close_paren)) break;
        Node *arg = conv(read_assignment_expr(p));
        Type *paramtype;
        if (i < vector_length(params)) {
            paramtype = vector_get(params, i++);
        } else {
            paramtype = 
                is_inttype(arg->ty) ? p->type_int :
                arg->ty;
        }
        ensure_assignable(paramtype, arg->ty);
        if (paramtype->kind != arg->ty->kind)
            arg = ast_conv(paramtype, arg);
        vector_push(args, arg);
        Token *tok = token(p);
        if (is_keyword(tok, close_paren)) break;
        if (!is_keyword(tok, comma))
            errort(tok, "unexpected token: '%s'", tok2s(tok));
    }
    return args;
}

static Node *read_funcall(parse p, Node *fp) {
    if (fp->kind == sym(addr) && fp->operand->kind == sym(funcdesg)) {
        Node *desg = fp->operand;
        vector args = read_func_args(p, desg->ty->params);
        return ast_funcall(desg->ty, desg->fname, args);
    }
    vector args = read_func_args(p, fp->ty->ptr->params);
    return ast_funcptr_call(fp, args);
}

static Node *read_var_or_func(parse p, buffer name) {
    Node *v = get(p->localenv, name);
    if (!v) {
        Token *tok = token(p);
        if (!is_keyword(tok, open_paren))
            errort(tok, "undefined variable: %s", name);
        Type *ty = make_func_type(p->type_int, allocate_vector(p->h, 10), false);
        warnt(tok, "assume returning int: %s()", name);
        return ast_funcdesg(ty, name);
    }
    if (v->ty->kind == sym(func))
        return ast_funcdesg(v->ty, name);
    return v;
}

static symbol get_compound_assign_op(Token *tok) {
    if (tok->kind != sym(keyword))
        return 0;
    return(tok->id);
}

static Node *read_compound_stmt(parse p);

static Node *read_stmt_expr(parse p) {
    Node *r = read_compound_stmt(p);
    expect(p, close_paren);
    Type *rtype = p->type_void;
    if (vector_length(r->stmts) > 0) {
        Node *lastexpr = vector_tail(r->stmts);
        if (lastexpr->ty)
            rtype = lastexpr->ty;
    }
    r->ty = rtype;
    return r;
}

static Node *read_primary_expr(parse p) {
    Token *tok = token(p);
    if (!tok) return NULL;
    if (is_keyword(tok, open_paren)) {
        if (next_token(p, open_bracket))
            return read_stmt_expr(p);
        Node *r = read_expr();
        expect(p, close_paren);
        return r;
    }
    if (tok->kind == sym(keyword)) {                  
        return NULL;
    }
    consume(p);
    if (tok->kind == sym(ident)) 
        return read_var_or_func(p, tok->sval);
    //    if (tok->kind == sym(number))     
    //        return read_int(p, tok);
    if (tok->kind == sym(char))         
        return ast_inttype(p, p->char_type, tok->c);
    if (tok->kind == sym(string))             
        return ast_string(p, tok->sval);

    error("internal error: unknown token kind: %d", tok->kind);
}

static Node *read_subscript_expr(parse p, Node *node) {
    Token *tok = token(p);
    Node *sub = read_expr();
    if (!sub)
        errort(tok, "subscription expected");
    expect(p, close_bracket);
    Node *t = binop(p, sym(+), conv(node), conv(sub));
    return ast_uop(sym(deref), t->ty->ptr, t);
}

static Node *read_struct_field(Node *struc);

static Node *read_postfix_expr_tail(parse p, Node *node) {
    if (!node) return NULL;
    for (;;) {
        if (next_token(p, open_paren)) {
            Token *tok = token(p);
            node = conv(node);
            Type *t = node->ty;
            if (t->kind != sym(ptr) || t->ptr->kind != sym(func))
                errort(tok, "function expected, but got %s", node2s(node));
            node = read_funcall(p, node);
            continue;
        }
        if (next_token(p, open_bracket)) {
            node = read_subscript_expr(p, node);
            continue;
        }
        if (next_token(p, sym(.))) {
            node = read_struct_field(node);
            continue;
        }
        if (next_token(p, sym(->))) {
            if (node->ty->kind != sym(ptr))
                error("pointer type expected, but got %s %s",
                      ty2s(node->ty), node2s(node));
            node = ast_uop(sym(deref), node->ty->ptr, node);
            node = read_struct_field(node);
            continue;
        }
        Token *tok = token(p);
        if (next_token(p, sym(inc)) || next_token(p, sym(dec))) {
            ensure_lvalue(node);
            symbol op = is_keyword(tok, sym(inc)) ? sym(post_inc) : sym(post_dec);
            return ast_uop(op, node->ty, node);
        }
        return node;
    }
}

static Node *read_postfix_expr(parse p) {
    Node *node = read_primary_expr(p);
    return read_postfix_expr_tail(p, node);
}

static Node *read_unary_expr(parse p);
 
static Node *read_unary_incdec(parse p, symbol op) {
    Node *operand = read_unary_expr(p);
    operand = conv(operand);
    ensure_lvalue(operand);
    return ast_uop(op, operand->ty, operand);
}

static Node *read_label_addr(parse p, Token *tok) {
    // [GNU] Labels as values. You can get the address of the a label
    // with unary "&&" operator followed by a label name.
    Token *tok2 = token(p);
    if (tok2->kind != sym(ident))
        errort(tok, "label name expected after &&, but got %s", tok2s(tok2));
    Node *r = ast_label_addr(p, tok2->sval);
    vector_push(gotos, r);
    return r;
}

static Node *read_unary_addr(parse p) {
    Node *operand = read_cast_expr(p);
    if (operand->kind == sym(funcdesg))
        return conv(operand);
    ensure_lvalue(operand);
    return ast_uop(sym(addr), make_ptr_type(operand->ty), operand);
}

static Node *read_unary_deref(Token *tok) {
    Node *operand = conv(read_cast_expr());
    if (operand->ty->kind != sym(ptr))
        errort(tok, "pointer type expected, but got %s", node2s(operand));
    if (operand->ty->ptr->kind == sym(func))
        return operand;
    return ast_uop(sym(deref), operand->ty->ptr, operand);
}

static Node *read_unary_minus() {
    Node *expr = read_cast_expr();
    ensure_arithtype(expr);
    if (is_inttype(expr->ty))
        return binop('-', conv(ast_inttype(expr->ty, 0)), conv(expr));
    return binop('-', ast_floattype(expr->ty, 0), expr);
}

static Node *read_unary_bitnot(Token *tok) {
    Node *expr = read_cast_expr();
    expr = conv(expr);
    if (!is_inttype(expr->ty))
        errort(tok, "invalid use of ~: %s", node2s(expr));
    return ast_uop('~', expr->ty, expr);
}

static Node *read_unary_lognot() {
    Node *operand = read_cast_expr();
    operand = conv(operand);
    return ast_uop('!', type_int, operand);
}

static Node *read_unary_expr(parse p) {
    Token *tok = get_token(p->b);
    if (tok->kind == sym(keyword)) {
        if (tok->id == sym(sizeof)) return read_sizeof_operand();
        if (tok->id == sym(alignof)) return read_alignof_operand();
        if (tok->id == sym(p))INC: return read_unary_incdec(OP_PRE_INC);
        if (tok->id == sym(p))DEC: return read_unary_incdec(OP_PRE_DEC);
        if (tok->id == sym(p))LOGAND: return read_label_addr(tok);
        if (tok->id == sym(&)) return read_unary_addr();
        if (tok->id == sym(*)) return read_unary_deref(tok);
        if (tok->id == sym(+)) return read_cast_expr();
        if (tok->id == sym(-)) return read_unary_minus();
        if (tok->id == sym(~)) return read_unary_bitnot(tok);
        if (tok->id == sym(!) return))read_unary_lognot();
    }
    unget_token(tok);
    return read_postfix_expr();
}

static Node *read_compound_literal(Type *ty) {
    buffer name = make_label();
    vector init = read_decl_init(ty);
    Node *r = ast_lvar(ty, name);
    r->lvarinit = init;
    return r;
}

static Node *read_cast_expr() {
    Token *tok = get();
    if (is_keyword(tok, open_paren) && is_type(p, peek())) {
        Type *ty = read_cast_type();
        expect(close_paren);
        if (is_keyword(peek(), '{')) {
            Node *node = read_compound_literal(ty);
            return read_postfix_expr_tail(node);
        }
        return ast_uop(OP_CAST, ty, read_cast_expr());
    }
    unget_token(tok);
    return read_unary_expr();
}

static Node *read_multiplicative_expr() {
    Node *node = read_cast_expr();
    for (;;) {
        if (next_token('*'))      node = binop('*', conv(node), conv(read_cast_expr()));
        else if (next_token('/')) node = binop('/', conv(node), conv(read_cast_expr()));
        else if (next_token('%')) node = binop('%', conv(node), conv(read_cast_expr()));
        else    return node;
    }
}

static Node *read_additive_expr() {
    Node *node = read_multiplicative_expr();
    for (;;) {
        if      (next_token('+')) node = binop('+', conv(node), conv(read_multiplicative_expr()));
        else if (next_token('-')) node = binop('-', conv(node), conv(read_multiplicative_expr()));
        else    return node;
    }
}

static Node *read_shift_expr() {
    Node *node = read_additive_expr();
    for (;;) {
        int op;
        if (next_token(OP_SAL))
            op = OP_SAL;
        else if (next_token(OP_SAR))
            op = node->ty->usig ? OP_SHR : OP_SAR;
        else
            break;
        Node *right = read_additive_expr();
        ensure_inttype(node);
        ensure_inttype(right);
        node = ast_binop(node->ty, op, conv(node), conv(right));
    }
    return node;
}

static Node *read_relational_expr() {
    Node *node = read_shift_expr();
    for (;;) {
        if      (next_token('<'))   node = binop('<',   conv(node), conv(read_shift_expr()));
        else if (next_token('>'))   node = binop('<',   conv(read_shift_expr()), conv(node));
        else if (next_token(OP_LE)) node = binop(OP_LE, conv(node), conv(read_shift_expr()));
        else if (next_token(OP_GE)) node = binop(OP_LE, conv(read_shift_expr()), conv(node));
        else    return node;
        node->ty = type_int;
    }
}

static Node *read_equality_expr() {
    Node *node = read_relational_expr();
    Node *r;
    if (next_token(OP_EQ)) {
        r = binop(OP_EQ, conv(node), conv(read_equality_expr()));
    } else if (next_token(OP_NE)) {
        r = binop(OP_NE, conv(node), conv(read_equality_expr()));
    } else {
        return node;
    }
    r->ty = type_int;
    return r;
}

static Node *read_bitand_expr() {
    Node *node = read_equality_expr();
    while (next_token('&'))
        node = binop('&', conv(node), conv(read_equality_expr()));
    return node;
}

static Node *read_bitxor_expr() {
    Node *node = read_bitand_expr();
    while (next_token('^'))
        node = binop('^', conv(node), conv(read_bitand_expr()));
    return node;
}

static Node *read_bitor_expr() {
    Node *node = read_bitxor_expr();
    while (next_token('|'))
        node = binop('|', conv(node), conv(read_bitxor_expr()));
    return node;
}

static Node *read_logand_expr() {
    Node *node = read_bitor_expr();
    while (next_token(OP_LOGAND))
        node = ast_binop(type_int, OP_LOGAND, node, read_bitor_expr());
    return node;
}

static Node *read_logor_expr() {
    Node *node = read_logand_expr();
    while (next_token(OP_LOGOR))
        node = ast_binop(type_int, OP_LOGOR, node, read_logand_expr());
    return node;
}

static Node *do_read_conditional_expr(Node *cond) {
    Node *then = conv(read_comma_expr());
    expect(':');
    Node *els = conv(read_conditional_expr());
    // [GNU] Omitting the middle operand is allowed.
    Type *t = then ? then->ty : cond->ty;
    Type *u = els->ty;
    // C11 6.5.15p5: if both types are arithemtic type, the result
    // type is the result of the usual arithmetic conversions.
    if (is_arithtype(t) && is_arithtype(u)) {
        Type *r = usual_arith_conv(t, u);
        return ast_ternary(r, cond, (then ? wrap(r, then) : NULL), wrap(r, els));
    }
    return ast_ternary(u, cond, then, els);
}

static Node *read_conditional_expr() {
    Node *cond = read_logor_expr();
    if (!next_token('?'))
        return cond;
    return do_read_conditional_expr(cond);
}

static Node *read_assignment_expr(parse p) {
    Node *node = read_logor_expr();
    Token *tok = get();
    if (!tok)
        return node;
    if (is_keyword(tok, '?'))
        return do_read_conditional_expr(node);
    int cop = get_compound_assign_op(tok);
    if (is_keyword(tok, '=') || cop) {
        Node *value = conv(read_assignment_expr());
        if (is_keyword(tok, '=') || cop)
            ensure_lvalue(node);
        Node *right = cop ? binop(cop, conv(node), value) : value;
        if (is_arithtype(node->ty) && node->ty->kind != right->ty->kind)
            right = ast_conv(node->ty, right);
        return ast_binop(node->ty, '=', node, right);
    }
    unget_token(tok);
    return node;
}

Node *read_expr() {
    Token *tok = peek();
    Node *r = read_comma_expr();
    if (!r)
        errort(tok, "expression expected");
    return r;
}

static Node *read_expr_opt() {
    return read_comma_expr();
}

static Node *read_struct_field(Node *struc) {
    // or union?
    if (struc->ty->kind != sym(struct))
        error("struct expected, but got %s", node2s(struc));
    Token *name = get();
    if (name->kind != TIDENT)
        error("field name expected, but got %s", tok2s(name));
    Type *field = dict_get(struc->ty->fields, name->sval);
    if (!field)
        error("struct has no such field: %s", tok2s(name));
    return ast_struct_ref(field, struc, name->sval);
}

static int compute_padding(int offset, int align) {
    return (offset % align == 0) ? 0 : align - offset % align;
}

static void squash_unnamed_struct(tuple dict, Type *unnamed, int offset) {
    vector keys = dict_keys(unnamed->fields);
    for (int i = 0; i < vector_len(keys); i++) {
        buffer name = vector_get(keys, i);
        Type *t = copy_type(dict_get(unnamed->fields, name));
        t->offset += offset;
        dict_put(dict, name, t);
    }
}

static int read_bitsize(buffer name, Type *ty) {
    if (!is_inttype(ty))
        error("non-integer type cannot be a bitfield: %s", ty2s(ty));
    Token *tok = peek();
    int r = read_intexpr(p);
    int maxsize = ty->kind == sym(boolean) ? 1 : ty->size * 8;
    if (r < 0 || maxsize < r)
        errort(tok, "invalid bitfield size for %s: %d", ty2s(ty), r);
    if (r == 0 && name != NULL)
        errort(tok, "zero-width bitfield needs to be unnamed: %s", name);
    return r;
}



static void assign_string(vector inits, Type *ty, buffer p, int off) {
    if (ty->len == -1)
        ty->len = ty->size = strlen(p) + 1;
    int i = 0;
    for (; i < ty->len && *p; i++)
        vector_push(inits, ast_init(ast_inttype(type_char, *p++), type_char, off + i));
    for (; i < ty->len; i++)
        vector_push(inits, ast_init(ast_inttype(type_char, 0), type_char, off + i));
}

static boolean maybe_read_brace() {
    return next_token('{');
}

static void maybe_skip_comma() {
    next_token(',');
}

static void skip_to_brace() {
    for (;;) {
        if (next_token('}'))
            return;
        if (next_token('.')) {
            get();
            expect('=');
        }
        Token *tok = peek();
        Node *ignore = read_assignment_expr();
        if (!ignore)
            return;
        warnt(tok, "excessive initializer: %s", node2s(ignore));
        maybe_skip_comma();
    }
}

static void read_initializer_elem(vector inits, Type *ty, int off, boolean designated) {
    next_token('=');
    if (ty->kind == sym(array) || ty->kind == sym(struct)) {
        read_initializer_list(inits, ty, off, designated);
    } else if (next_token('{')) {
        read_initializer_elem(inits, ty, off, true);
        expect('}');
    } else {
        Node *expr = conv(read_assignment_expr());
        ensure_assignable(ty, expr->ty);
        vector_push(inits, ast_init(expr, ty, off));
    }
}

static int comp_init(const void *p, const void *q) {
    int x = (*(Node **)p)->initoff;
    int y = (*(Node **)q)->initoff;
    if (x < y) return -1;
    if (x > y) return 1;
    return 0;
}

static void sort_inits(vector inits) {
    qsort(vector_body(inits), vector_len(inits), sizeof(void *), comp_init);
}

static void read_struct_initializer_sub(vector inits, Type *ty, int off, boolean designated) {
    boolean has_brace = maybe_read_brace();
    vector keys = dict_keys(ty->fields);
    int i = 0;
    for (;;) {
        Token *tok = get();
        if (is_keyword(tok, '}')) {
            if (!has_brace)
                unget_token(tok);
            return;
        }
        buffer fieldname;
        Type *fieldtype;
        if ((is_keyword(tok, '.') || is_keyword(tok, '[')) && !has_brace && !designated) {
            unget_token(tok);
            return;
        }
        if (is_keyword(tok, '.')) {
            tok = get();
            if (!tok || tok->kind != TIDENT)
                errort(tok, "malformed desginated initializer: %s", tok2s(tok));
            fieldname = tok->sval;
            fieldtype = dict_get(ty->fields, fieldname);
            if (!fieldtype)
                errort(tok, "field does not exist: %s", tok2s(tok));
            keys = dict_keys(ty->fields);
            i = 0;
            while (i < vector_len(keys)) {
                buffer s = vector_get(keys, i++);
                if (strcmp(fieldname, s) == 0)
                    break;
            }
            designated = true;
        } else {
            unget_token(tok);
            if (i == vector_len(keys))
                break;
            fieldname = vector_get(keys, i++);
            fieldtype = dict_get(ty->fields, fieldname);
        }
        read_initializer_elem(inits, fieldtype, off + fieldtype->offset, designated);
        maybe_skip_comma();
        designated = false;
        if (!ty->is_struct)
            break;
    }
    if (has_brace)
        skip_to_brace();
}

static void read_struct_initializer(vector inits, Type *ty, int off, boolean designated) {
    read_struct_initializer_sub(inits, ty, off, designated);
    sort_inits(inits);
}

static void read_array_initializer_sub(vector inits, Type *ty, int off, boolean designated) {
    boolean has_brace = maybe_read_brace();
    boolean flexible = (ty->len <= 0);
    int elemsize = ty->ptr->size;
    int i;
    for (i = 0; flexible || i < ty->len; i++) {
        Token *tok = get();
        if (is_keyword(tok, '}')) {
            if (!has_brace)
                unget_token(tok);
            goto finish;
        }
        if ((is_keyword(tok, '.') || is_keyword(tok, '[')) && !has_brace && !designated) {
            unget_token(tok);
            return;
        }
        if (is_keyword(tok, '[')) {
            Token *tok = peek();
            int idx = read_intexpr(p);
            if (idx < 0 || (!flexible && ty->len <= idx))
                errort(tok, "array designator exceeds array bounds: %d", idx);
            i = idx;
            expect(']');
            designated = true;
        } else {
            unget_token(tok);
        }
        read_initializer_elem(inits, ty->ptr, off + elemsize * i, designated);
        maybe_skip_comma();
        designated = false;
    }
    if (has_brace)
        skip_to_brace();
 finish:
    if (ty->len < 0) {
        ty->len = i;
        ty->size = elemsize * i;
    }
}

static void read_array_initializer(vector inits, Type *ty, int off, boolean designated) {
    read_array_initializer_sub(inits, ty, off, designated);
    sort_inits(inits);
}

static boolean is_string(Type *ty) {
    return ty->kind == sym(array) && ty->ptr->kind == sym(char);
}

static void read_initializer_list(vector inits, Type *ty, int off, boolean designated) {
    Token *tok = get();
    if (is_string(ty)) {
        if (tok->kind == TSTRING) {
            assign_string(inits, ty, tok->sval, off);
            return;
        }
        if (is_keyword(tok, '{') && peek()->kind == TSTRING) {
            tok = get();
            assign_string(inits, ty, tok->sval, off);
            expect('}');
            return;
        }
    }
    unget_token(tok);
    if (ty->kind == sym(array)) {
        read_array_initializer(inits, ty, off, designated);
    } else if (ty->kind == sym(struct)) {
        read_struct_initializer(inits, ty, off, designated);
    } else {
        Type *arraytype = make_array_type(ty, 1);
        read_array_initializer(inits, arraytype, off, designated);
    }
}

static vector read_decl_init(Type *ty) {
    vector r = allocate_vector(h);
    if (is_keyword(peek(), '{') || is_string(ty)) {
        read_initializer_list(r, ty, 0, false);
    } else {
        Node *init = conv(read_assignment_expr());
        if (is_arithtype(init->ty) && init->ty->kind != ty->kind)
            init = ast_conv(ty, init);
        vector_push(r, ast_init(init, ty, 0));
    }
    return r;
}

static Type *read_func_param(buffer *name, boolean optional) {
    int sclass = 0;
    Type *basety = type_int;
    if (is_type(p, peek())) {
        basety = read_decl_spec(p, &sclass);
    } else if (optional) {
        errort(peek(), "type expected, but got %s", tok2s(peek()));
    }
    Type *ty = read_declarator(name, basety, NULL, optional ? DECL_PARAM_TYPEONLY : DECL_PARAM);
    // C11 6.7.6.3p7: Array of T is adjusted to pointer to T
    // in a function parameter list.
    if (ty->kind == sym(array))
        return make_ptr_type(ty->ptr);
    // C11 6.7.6.3p8: Function is adjusted to pointer to function
    // in a function parameter list.
    if (ty->kind == sym(func))
        return make_ptr_type(ty);
    return ty;
}

// Reads an ANSI-style prototyped function parameter list.
static void read_declarator_params(vector types, vector vars, boolean *ellipsis) {
    boolean typeonly = !vars;
    *ellipsis = false;
    for (;;) {
        Token *tok = peek();
        if (next_token(KELLIPSIS)) {
            if (vector_len(types) == 0)
                errort(tok, "at least one parameter is required before \"...\"");
            expect(close_paren);
            *ellipsis = true;
            return;
        }
        buffer name;
        Type *ty = read_func_param(&name, typeonly);
        ensure_not_void(ty);
        vector_push(types, ty);
        if (!typeonly)
            vector_push(vars, ast_lvar(ty, name));
        tok = get();
        if (is_keyword(tok, close_paren))
            return;
        if (!is_keyword(tok, ','))
            errort(tok, "comma expected, but got %s", tok2s(tok));
    }
}

static Type *read_func_param_list(vector paramvars, Type *rettype) {
    // C11 6.7.6.3p10: A parameter list with just "void" specifies that
    // the function has no parameters.
    Token *tok = get();
    if (is_keyword(tok, KVOID) && next_token(close_paren))
        return make_func_type(rettype, allocate_vector(h), false, false);

    // C11 6.7.6.3p14: K&R-style un-prototyped declaration or
    // function definition having no parameters.
    // We return a type representing K&R-style declaration here.
    // If this is actually part of a declartion, the type will be fixed later.
    if (is_keyword(tok, close_paren))
        return make_func_type(rettype, allocate_vector(h), true, true);
    unget_token(tok);

    Token *tok2 = peek();
    if (next_token(KELLIPSIS))
        errort(tok2, "at least one parameter is required before \"...\"");
    if (is_type(p, peek())) {
        boolean ellipsis;
        vector paramtypes = allocate_vector(h);
        read_declarator_params(paramtypes, paramvars, &ellipsis);
        return make_func_type(rettype, paramtypes, ellipsis, false);
    }
    if (!paramvars)
        errort(tok, "invalid function definition");
    vector paramtypes = allocate_vector(h);
    for (int i = 0; i < vector_len(paramvars); i++)
        vector_push(paramtypes, type_int);
    return make_func_type(rettype, paramtypes, false, true);
}

static Type *read_declarator_array(Type *basety) {
    int len;
    if (next_token(']')) {
        len = -1;
    } else {
        len = read_intexpr(p);
        expect(']');
    }
    Token *tok = peek();
    Type *t = read_declarator_tail(basety, NULL);
    if (t->kind == sym(func))
        errort(tok, "array of functions");
    return make_array_type(t, len);
}

static Type *read_declarator_func(Type *basety, vector param) {
    if (basety->kind == sym(func))
        error("function returning a function");
    if (basety->kind == sym(array))
        error("function returning an array");
    return read_func_param_list(param, basety);
}

static Type *read_declarator_tail(Type *basety, vector params) {
    if (next_token('['))
        return read_declarator_array(basety);
    if (next_token(open_paren))
        return read_declarator_func(basety, params);
    return basety;
}

static void skip_type_qualifiers() {
    while (next_token(KCONST) || next_token(KVOLATILE) || next_token(KRESTRICT));
}

// C11 6.7.6: Declarators
static Type *read_declarator(buffer *rname, Type *basety, vector params, int ctx) {
    if (next_token(open_paren)) {
        // open_paren is either beginning of grouping parentheses or of a function parameter list.
        // If the next token is a type name, a parameter list must follow.
        if (is_type(p, peek()))
            return read_declarator_func(basety, params);
        // If not, it's grouping. In that case we have to read from outside.
        // For example, consider int (*)(), which is "pointer to function returning int".
        // We have only read "int" so far. We don't want to pass "int" to
        // a recursive call, or otherwise we would get "pointer to int".
        // Here, we pass a dummy object to get "pointer to <something>" first,
        // continue reading to get "function returning int", and then combine them.
        Type *stub = make_stub_type();
        Type *t = read_declarator(rname, stub, params, ctx);
        expect(close_paren);
        *stub = *read_declarator_tail(basety, params);
        return t;
    }
    if (next_token('*')) {
        skip_type_qualifiers();
        return read_declarator(rname, make_ptr_type(basety), params, ctx);
    }
    Token *tok = get();
    if (tok->kind == TIDENT) {
        if (ctx == DECL_CAST)
            errort(tok, "identifier is not expected, but got %s", tok2s(tok));
        *rname = tok->sval;
        return read_declarator_tail(basety, params);
    }
    if (ctx == DECL_BODY || ctx == DECL_PARAM)
        errort(tok, "identifier, ( or * are expected, but got %s", tok2s(tok));
    unget_token(tok);
    return read_declarator_tail(basety, params);
}


static int read_alignas() {
    // C11 6.7.5. Valid form of _Alignof is either _Alignas(type-name) or
    // _Alignas(constant-expression).
    expect(open_paren);
    int r = is_type(p, peek())
        ? read_cast_type()->align
        : read_intexpr(p);
    expect(close_paren);
    return r;
}

static void read_static_local_var(Type *ty, buffer name) {
    Node *var = ast_static_lvar(ty, name);
    vector init = NULL;
    if (next_token('=')) {
        tuple *orig = localenv;
        localenv = NULL;
        init = read_decl_init(ty);
        localenv = orig;
    }
    vector_push(toplevels, ast_decl(var, init));
}

static Type *read_decl_spec_opt(int *sclass) {
    if (is_type(p, peek()))
        return read_decl_spec(p, sclass);
    warnt(peek(), "type specifier missing, assuming int");
    return type_int;
}

static void read_decl(vector block, boolean isglobal) {
    int sclass = 0;
    Type *basetype = read_decl_spec_opt(p, &sclass);
    if (next_token(';'))
        return;
    for (;;) {
        buffer name = NULL;
        Type *ty = read_declarator(&name, copy_incomplete_type(basetype), NULL, DECL_BODY);
        ty->isstatic = (sclass == S_STATIC);
        if (sclass == symbol(typedef)) {
            Node *r = make_ast(&(Node){ sym(typedef), ty }, 0);
            set(env, name, r);            
        } else if (ty->isstatic && !isglobal) {
            ensure_not_void(ty);
            read_static_local_var(ty, name);
        } else {
            ensure_not_void(ty);
            Node *var = (isglobal ? ast_gvar : ast_lvar)(ty, name);
            if (next_token('=')) {
                vector_push(block, ast_decl(var, read_decl_init(ty)));
            } else if (sclass != S_EXTERN && ty->kind != KIND_FUNC) {
                vector_push(block, ast_decl(var, NULL));
            }
        }
        if (next_token(';'))
            return;
        if (!next_token(','))
            errort(peek(), "';' or ',' are expected, but got %s", tok2s(peek()));
    }
}

static vector param_types(vector params) {
    vector r = allocate_vector(h);
    for (int i = 0; i < vector_len(params); i++) {
        Node *param = vector_get(params, i);
        vector_push(r, param->ty);
    }
    return r;
}

static Node *read_func_body(Type *functype, buffer fname, vector params) {
    localenv = make_map_parent(localenv);
    localvars = allocate_vector(h);
    current_func_type = functype;
    Node *funcname = ast_string(ENC_NONE, fname, strlen(fname) + 1);
    set(localenv, "__func__", funcname);
    set(localenv, "__FUNCTION__", funcname);
    Node *body = read_compound_stmt();
    Node *r = ast_func(functype, fname, params, body, localvars);
    current_func_type = NULL;
    localenv = NULL;
    localvars = NULL;
    return r;
}

static void skip_parentheses(vector buf) {
    for (;;) {
        Token *tok = get();
        if (tok->kind == TEOF)
            error("premature end of input");
        vector_push(buf, tok);
        if (is_keyword(tok, close_paren))
            return;
        if (is_keyword(tok, open_paren))
            skip_parentheses(buf);
    }
}

// is_funcdef returns true if we are at beginning of a function definition.
// The basic idea is that if we see '{' or a type keyword after a closing
// parenthesis of a function parameter list, we were reading a function
// definition. (Usually '{' comes after a closing parenthesis.
// A type keyword is allowed for K&R-style function definitions.)
static boolean is_funcdef() {
    vector buf = allocate_vector(h);
    boolean r = false;
    for (;;) {
        Token *tok = get();
        vector_push(buf, tok);
        if (tok->kind == TEOF)
            error("premature end of input");
        if (is_keyword(tok, ';'))
            break;
        if (is_type(p, tok))
            continue;
        if (is_keyword(tok, open_paren)) {
            skip_parentheses(buf);
            continue;
        }
        if (tok->kind != TIDENT)
            continue;
        if (!is_keyword(peek(), open_paren))
            continue;
        vector_push(buf, get());
        skip_parentheses(buf);
        r = (is_keyword(peek(), '{') || is_type(p, peek()));
        break;
    }
    while (vector_len(buf) > 0)
        unget_token(vector_pop(buf));
    return r;
}

static void backfill_labels() {
    for (int i = 0; i < vector_len(gotos); i++) {
        Node *src = vector_get(gotos, i);
        buffer label = src->label;
        Node *dst = get(labels, label);
        if (!dst)
            error("stray %s: %s", src->kind == sym(goto) ? "goto" : "unary &&", label);
        if (dst->newlabel)
            src->newlabel = dst->newlabel;
        else
            src->newlabel = dst->newlabel = make_label();
    }
}

static Node *read_funcdef(parse p) {
    int sclass = 0;
    Type *basetype = read_decl_spec_opt(p, &sclass);
    localenv = make_map_parent(globalenv);
    gotos = allocate_vector(h);
    labels = make_map();
    buffer name;
    vector params = allocate_vector(h);
    Type *functype = read_declarator(&name, basetype, params, DECL_BODY);
    functype->isstatic = (sclass == S_STATIC);
    ast_gvar(functype, name);
    expect('{');
    Node *r = read_func_body(functype, name, params);
    backfill_labels();
    localenv = NULL;
    return r;
}

static Node *read_boolean_expr() {
    Node *cond = read_expr();
    return cond;
}

static Node *read_if_stmt(parse p) {
    expect(p, open_paren);
    Node *cond = read_boolean_expr();
    expect(close_paren);
    Node *then = read_stmt();
    if (!next_token(KELSE))
        return ast_if(cond, then, NULL);
    Node *els = read_stmt();
    return ast_if(cond, then, els);
}

static Node *read_opt_decl_or_stmt(parse p) {
    if (next_token(';'))
        return NULL;
    vector list = allocate_vector(h);
    read_decl_or_stmt(list);
    return ast_compound_stmt(list);
}

#define SET_JUMP_LABELS(cont, brk)              \
    buffer ocontinue = lcontinue;                \
    buffer obreak = lbreak;                      \
    lcontinue = cont;                           \
    lbreak = brk

#define RESTORE_JUMP_LABELS()                   \
    lcontinue = ocontinue;                      \
    lbreak = obreak

static Node *read_for_stmt() {
    expect(open_paren);
    buffer beg = make_label();
    buffer mid = make_label();
    buffer end = make_label();
    tuple *orig = localenv;
    localenv = make_map_parent(localenv);
    Node *init = read_opt_decl_or_stmt();
    Node *cond = read_expr_opt();
    expect(';');
    Node *step = read_expr_opt();
    expect(close_paren);
    SET_JUMP_LABELS(mid, end);
    Node *body = read_stmt();
    RESTORE_JUMP_LABELS();
    localenv = orig;

    vector v = allocate_vector(h);
    if (init)
        vector_push(v, init);
    vector_push(v, ast_dest(beg));
    if (cond)
        vector_push(v, ast_if(cond, NULL, ast_jump(end)));
    if (body)
        vector_push(v, body);
    vector_push(v, ast_dest(mid));
    if (step)
        vector_push(v, step);
    vector_push(v, ast_jump(beg));
    vector_push(v, ast_dest(end));
    return ast_compound_stmt(v);
}


static Node *read_while_stmt(parse p) {
    expect(open_paren);
    Node *cond = read_boolean_expr();
    expect(close_paren);

    buffer beg = make_label();
    buffer end = make_label();
    SET_JUMP_LABELS(beg, end);
    Node *body = read_stmt();
    RESTORE_JUMP_LABELS();

    vector v = allocate_vector(h);
    vector_push(v, ast_dest(beg));
    vector_push(v, ast_if(cond, body, ast_jump(end)));
    vector_push(v, ast_jump(beg));
    vector_push(v, ast_dest(end));
    return ast_compound_stmt(v);
}

static Node *read_do_stmt(parse p) {
    buffer beg = make_label();
    buffer end = make_label();
    SET_JUMP_LABELS(beg, end);
    Node *body = read_stmt();
    RESTORE_JUMP_LABELS();
    Token *tok = get();
    if (!is_keyword(tok, KWHILE))
        errort(tok, "'while' is expected, but got %s", tok2s(tok));
    expect(open_paren);
    Node *cond = read_boolean_expr();
    expect(close_paren);
    expect(';');

    vector v = allocate_vector(h);
    vector_push(v, ast_dest(beg));
    if (body)
        vector_push(v, body);
    vector_push(v, ast_if(cond, ast_jump(beg), NULL));
    vector_push(v, ast_dest(end));
    return ast_compound_stmt(v);
}

static Node *make_switch_jump(Node *var, Case *c) {
    Node *cond;
    if (c->beg == c->end) {
        cond = ast_binop(type_int, OP_EQ, var, ast_inttype(type_int, c->beg));
    } else {
        // [GNU] case i ... j is compiled to if (i <= cond && cond <= j) goto <label>.
        Node *x = ast_binop(type_int, sym(<=), ast_inttype(type_int, c->beg), var);
        Node *y = ast_binop(type_int, sym(<=), var, ast_inttype(type_int, c->end));
        cond = ast_binop(type_int, sym(logand), x, y);
    }
    return ast_if(cond, ast_jump(c->label), NULL);
}

// C11 6.8.4.2p3: No two case constant expressions have the same value.
static void check_case_duplicates(vector cases) {
    int len = vector_len(cases);
    Case *x = vector_get(cases, len - 1);
    for (int i = 0; i < len - 1; i++) {
        Case *y = vector_get(cases, i);
        if (x->end < y->beg || y->end < x->beg)
            continue;
        if (x->beg == x->end)
            error("duplicate case value: %d", x->beg);
        error("duplicate case value: %d ... %d", x->beg, x->end);
    }
}

// this is only two levels deep in the original code?

#define SET_SWITCH_CONTEXT(p, brk)             \
    p->ocases = p->cases;                      \
    p->odefaultcase = p->defaultcase;          \
    p->obreak = p->break;                      \
    p->cases = p->allocate_vector(h);          \
    p->defaultcase = NULL;                     \
    p->lbreak = brk

#define RESTORE_SWITCH_CONTEXT(p)              \
    p->cases = p->ocases;                      \
    p->defaultcase = p->odefaultcase;          \
    p->lbreak = p->obreak

static Node *read_switch_stmt(parse p)
{
    expect(p, open_paren);
    Node *expr = conv(read_expr());
    ensure_inttype(expr);
    expect(p, close_paren);

    buffer end = make_label();
    SET_SWITCH_CONTEXT(p, end);
    Node *body = read_stmt();
    vector v = allocate_vector(h);
    Node *var = ast_lvar(expr->ty, make_tempname());
    vector_push(v, ast_binop(expr->ty, '=', var, expr));
    for (int i = 0; i < vector_len(cases); i++)
        vector_push(v, make_switch_jump(var, vector_get(cases, i)));
    vector_push(v, ast_jump(defaultcase ? defaultcase : end));
    if (body)
        vector_push(v, body);
    vector_push(v, ast_dest(end));
    RESTORE_SWITCH_CONTEXT(p);
    return ast_compound_stmt(v);
}

static Node *read_label_tail(Node *label) {
    Node *stmt = read_stmt();
    vector v = allocate_vector(h);
    vector_push(v, label);
    if (stmt)
        vector_push(v, stmt);
    return ast_compound_stmt(v);
}

static Node *read_case_label(parse p, Token *tok) {
    if (!cases)
        errort(tok, "stray case label");
    buffer label = make_label();
    int beg = read_intexpr(p);
    if (next_token(KELLIPSIS)) {
        int end = read_intexpr(p);
        expect(':');
        if (beg > end)
            errort(tok, "case region is not in correct order: %d ... %d", beg, end);
        vector_push(cases, make_case(beg, end, label));
    } else {
        expect(':');
        vector_push(cases, make_case(beg, beg, label));
    }
    check_case_duplicates(cases);
    return read_label_tail(ast_dest(label));
}

static Node *read_default_label(parse p, Token *tok) {
    expect(':');
    if (defaultcase)
        errort(tok, "duplicate default");
    defaultcase = make_label();
    return read_label_tail(ast_dest(defaultcase));
}

static Node *read_break_stmt(parse p, Token *tok) {
    expect(';');
    if (!lbreak)
        errort(tok, "stray break statement");
    return ast_jump(lbreak);
}

static Node *read_continue_stmt(parse p, Token *tok) {
    expect(';');
    if (!lcontinue)
        errort(tok, "stray continue statement");
    return ast_jump(lcontinue);
}

static Node *read_return_stmt(parse p) {
    Node *retval = read_expr_opt();
    expect(';');
    if (retval)
        return ast_return(ast_conv(current_func_type->rettype, retval));
    return ast_return(NULL);
}

static Node *read_goto_stmt(parse p) {
    if (next_token('*')) {
        // [GNU] computed goto. "goto *p" jumps to the address pointed by p.
        Token *tok = peek();
        Node *expr = read_cast_expr();
        if (expr->ty->kind != sym(ptr))
            errort(tok, "pointer expected for computed goto, but got %s", node2s(expr));
        return ast_computed_goto(expr);
    }
    Token *tok = get();
    if (!tok || tok->kind != sym(ident))
        errort(tok, "identifier expected, but got %s", tok2s(tok));
    expect(';');
    Node *r = ast_goto(tok->sval);
    vector_push(gotos, r);
    return r;
}

static Node *read_label(Token *tok)
{
    buffer label = tok->sval;
    if (get(labels, label))
        errort(tok, "duplicate label: %s", tok2s(tok));
    Node *r = ast_label(label);
    set(labels, label, r);
    return read_label_tail(r);
}

static Node *read_stmt(parse p) {
    Token *tok = get_token(p);
    if (tok->kind == TKEYWORD) {
        if (tok->id == intern(sstring("{"))) read_compound_stmt(p);
        if (tok->id == sym(if)) read_if_stmt(p);
        if (tok->id == sym(for)) read_for_stmt(p);            
        if (tok->id == sym(while))    return read_while_stmt(p);
        if (tok->id == sym(do))       return read_do_stmt(p);
        if (tok->id == sym(return))   return read_return_stmt(p);
        if (tok->id == sym(switch))   return read_switch_stmt(p);
        if (tok->id == sym(case))     return read_case_label(p, tok);
        if (tok->id == sym(default))  return read_default_label(p, tok);
        if (tok->id == sym(break))    return read_break_stmt(p, tok);
        if (tok->id == sym(continue)) return read_continue_stmt(p, tok);
        if (tok->id == sym(goto))     return read_goto_stmt(p);
    }
    if ((tok->kind == sym(ident)) && next_token(':'))
        return read_label(tok);

    
    unget_token(tok);
    Node *r = read_expr_opt();
    expect(';');
    return r;
}

static Node *read_compound_stmt() {
    tuple *orig = localenv;
    localenv = make_map_parent(localenv);
    vector list = allocate_vector(h);
    for (;;) {
        if (next_token('}'))
            break;
        read_decl_or_stmt(list);
    }
    localenv = orig;
    return ast_compound_stmt(list);
}

static void read_decl_or_stmt(vector list) {
    Token *tok = peek();
    if (tok->kind == TEOF)
        error("premature end of input");
    mark_location();
    if (is_type(p, tok)) {
        read_decl(list, false);
    } else if (next_token(KSTATIC_ASSERT)) {
        read_static_assert();
    } else {
        Node *stmt = read_stmt();
        if (stmt)
            vector_push(list, stmt);
    }
}

vector read_toplevels(parse p) {
    toplevels = allocate_vector(h);
    for (;;) {
        if (peek()->kind == TEOF)
            return toplevels;
        if (is_funcdef())
            vector_push(toplevels, read_funcdef());
        else
            read_decl(toplevels, true);
    }
}

// C11 5.1.1.2p6 Adjacent string literal tokens are concatenated.
static void concatenate_string(Token *tok) {
    int enc = tok->enc;
    buffer b = make_buffer();
    buffer_push(b, tok->sval);
    while (peek()->kind == sym(string)) {
        Token *tok2 = read_token();
        buf_append(b, tok2->sval, tok2->slen - 1);
    }
    tok->sval = b;
}

static Token *token_get(parse p) {
    Token *r = read_token();
    if (r->kind == TINVALID)
        errort(r, "stray character in program: '%c'", r->c);
    if (r->kind == sym(string) && peek()->kind == sym(string))
        concatenate_string(r);
    return r;
}

static void define_builtin(buffer name, Type *rettype, vector paramtypes) {
    ast_gvar(make_func_type(rettype, paramtypes, true, false), name);
}

parse parse_init(heap h) {
    vector voidptr = make_vector1(make_ptr_type(type_void));
    vector two_voidptrs = allocate_vector(h);
    vector_push(two_voidptrs, make_ptr_type(type_void));
    vector_push(two_voidptrs, make_ptr_type(type_void));
    define_builtin("__builtin_return_address", make_ptr_type(type_void), voidptr);
    define_builtin("__builtin_reg_class", type_int, voidptr);
    define_builtin("__builtin_va_arg", type_void, two_voidptrs);
    define_builtin("__builtin_va_start", type_void, voidptr);
    type_void = &(Type){ sym(void), 0, 0, false };
    type_bool = &(Type){ sym(boolean), 1, 1, true };
    type_char = &(Type){ sym(char), 1, 1, false };
    type_short = &(Type){ sym(short), 2, 2, false };
    type_int = &(Type){ sym(int), 4, 4, false };
    type_long = &(Type){ sym(long), 8, 8, false };
    type_llong = &(Type){ sym(llong), 8, 8, false };
    type_uchar = &(Type){ sym(char), 1, 1, true };
    type_ushort = &(Type){ sym(short), 2, 2, true };
    type_uint = &(Type){ sym(int), 4, 4, true };
    type_ulong = &(Type){ sym(long), 8, 8, true };
    type_ullong = &(Type){ sym(llong), 8, 8, true };
    type_enum = &(Type){ sym(enum), 4, 4, false };
    open_paren = intern(sstring("("))
    close_paren = intern(sstring(")"));
    open_brace = intern(sstring("{"))        
    close_brace = intern(sstring("}"));
    open_bracket = intern(sstring("["))        
    close_bracket = intern(sstring("]"));    
    comma = intern(sstring(","))
    semicolon = intern(sstring(";"))
    colon = intern(sstring(":"))                        
}
