// Copyright 2012 Rui Ueyama. Released under the MIT license.
#include "nc.h"

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

Type *lookup_field(Type *t, symbol s)
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

static Node *ast_inttype(parse p, Type *ty, long val) {
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
    return make_type(&(Type){ sym(ptr), .ptr = ty, .size = 8, .align = 8 });
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
    return 0; 
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
    if (!is_inttype(node->ty))
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
    // typespace
    return false;
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
    assert(is_inttype(t));
    assert(is_inttype(u));
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
    assert(is_inttype(lhs->ty));
    assert(is_inttype(rhs->ty));
    Type *r = usual_arith_conv(p, lhs->ty, rhs->ty);
    return ast_binop(p, r, op, wrap(r, lhs), wrap(r, rhs));
}

static void ensure_assignable(Type *totype, Type *fromtype) {
    if ((is_inttype(totype) || totype->kind == sym(ptr)) &&
        (is_inttype(fromtype) || fromtype->kind == sym(ptr)))
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

static Type *read_declarator(parse p, buffer *rname, Type *basety, vector params, int ctx);

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
    return 0;
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
        Node *constval = ast_inttype(p, p->type_int, val++);
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
    vector fields = read_rectype_fields(p, &size, &align);
    r->align = align;
    if (fields) {
        r->fields = fields;
        r->size = size;
    }
    return r;
}


static int read_bitsize(parse p, buffer name, Type *ty) {
    if (!is_inttype(ty))
        error("non-integer type cannot be a bitfield: %s", ty2s(ty));
    consume(p);
    int r = read_intexpr(p);
    int maxsize = ty->kind == sym(boolean) ? 1 : ty->size * 8;
    if (r < 0 || maxsize < r)
        errort(tok, "invalid bitfield size for %s: %d", ty2s(ty), r);
    if (r == 0 && name != NULL)
        errort(tok, "zero-width bitfield needs to be unnamed: %s", name);
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
            Type *fieldtype = read_declarator(p, &name, basetype, NULL, DECL_PARAM_TYPEONLY);
            ensure_not_void(fieldtype);
            fieldtype = copy_type(fieldtype);
            fieldtype->bitsize = next_token(p, colon) ? read_bitsize(p, name, fieldtype) : -1;
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

static Node *read_unary_expr(parse p);

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
    return ast_inttype(p, p->type_ulong, size);
}

static Node *read_alignof_operand(parse p) {
    expect(p, open_paren);
    Type *ty = read_cast_type(p);
    expect(p, close_paren);
    return ast_inttype(p, p->type_ulong, ty->align);
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
static Node *read_stmt(parse p);

static Node *vector_tail(vector r)
{
}


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

Node *read_expr(parse p) {
    consume(p);
    Node *r = read_comma_expr(p);
    if (!r)
        errort(tok, "expression expected");
    return r;
}

static Node *read_primary_expr(parse p) {
    Token *tok = token(p);
    if (!tok) return NULL;
    if (is_keyword(tok, open_paren)) {
        if (next_token(p, open_bracket))
            return read_stmt_expr(p);
        Node *r = read_expr(p);
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
        return ast_inttype(p, p->type_char, tok->c);
    if (tok->kind == sym(string))             
        return ast_string(p, tok->sval);

    error("internal error: unknown token kind: %d", tok->kind);
}


static Node *read_subscript_expr(parse p, Node *node) {
    //    Token *tok = token(p);
    consume(p);
    Node *sub = read_expr(p);
    if (!sub)
        errort(tok, "subscription expected");
    expect(p, close_bracket);
    Node *t = binop(p, sym(+), conv(node), conv(sub));
    return ast_uop(sym(deref), t->ty->ptr, t);
}

static Node *read_struct_field(parse p, Node *struc);

static Node *read_postfix_expr_tail(parse p, Node *node) {
    if (!node) return NULL;
    for (;;) {
        if (next_token(p, open_paren)) {
            consume(p);
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
            node = read_struct_field(p, node);
            continue;
        }
        if (next_token(p, sym(->))) {
            if (node->ty->kind != sym(ptr))
                error("pointer type expected, but got %s %s",
                      ty2s(node->ty), node2s(node));
            node = ast_uop(sym(deref), node->ty->ptr, node);
            node = read_struct_field(p, node);
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
    vector_push(p->gotos, r);
    return r;
}

static Node *read_cast_expr(parse p);

static Node *read_unary_addr(parse p) {
    Node *operand = read_cast_expr(p);
    if (operand->kind == sym(funcdesg))
        return conv(operand);
    ensure_lvalue(operand);
    return ast_uop(sym(addr), make_ptr_type(operand->ty), operand);
}

static Node *read_unary_deref(parse p, Token *tok) {
    Node *operand = conv(read_cast_expr(p));
    if (operand->ty->kind != sym(ptr))
        errort(tok, "pointer type expected, but got %s", node2s(operand));
    if (operand->ty->ptr->kind == sym(func))
        return operand;
    return ast_uop(sym(deref), operand->ty->ptr, operand);
}

static Node *read_unary_minus(parse p) {
    Node *expr = read_cast_expr(p);
    ensure_inttype(expr);
    return binop(p, sym(-), conv(ast_inttype(p, expr->ty, 0)), conv(expr));
}

static Node *read_unary_bitnot(parse p, Token *tok) {
    Node *expr = read_cast_expr(p);
    expr = conv(expr);
    if (!is_inttype(expr->ty))
        errort(tok, "invalid use of ~: %s", node2s(expr));
    return ast_uop(sym(~), expr->ty, expr);
}

static Node *read_unary_lognot(parse p) {
    Node *operand = read_cast_expr(p);
    operand = conv(operand);
    return ast_uop(sym(!), p->type_int, operand);
}

static Node *read_unary_expr(parse p) {
    Token *tok = get_token(p->b);
    if (tok->kind == sym(keyword)) {
        if (tok->id == sym(sizeof)) return read_sizeof_operand(p);
        if (tok->id == sym(alignof)) return read_alignof_operand(p);
        if (tok->id == sym(pre_inc)) return read_unary_incdec(p, sym(pre_inc));
        if (tok->id == sym(pre_dec)) return read_unary_incdec(p, sym(pre_dec));
        // computed goto?
        if (tok->id == sym(&&)) return read_label_addr(p, tok);
        if (tok->id == sym(&)) return read_unary_addr(p);
        if (tok->id == sym(*)) return read_unary_deref(p, tok);
        if (tok->id == sym(+)) return read_cast_expr(p);
        if (tok->id == sym(-)) return read_unary_minus(p);
        if (tok->id == sym(~)) return read_unary_bitnot(p, tok);
        if (tok->id == sym(!)) return read_unary_lognot(p);
    }
    // unget_token(tok);
    return read_postfix_expr(p);
}

static boolean is_string(Type *ty) {
    return ty->kind == sym(array) && ty->ptr->kind == sym(char);
}


static void assign_string(parse p, vector inits, Type *ty, buffer s, int off) {
}
#if 0
    if (ty->len == -1)
        ty->len = ty->size = buffer_length(s);
    int i = 0;
    for (; i < ty->len && *p; i++)
        vector_push(inits, ast_init(ast_inttype(p, p->type_char, *p++), p->type_char, off + i));
    for (; i < ty->len; i++)
        vector_push(inits, ast_init(ast_inttype(p, p->type_char, 0), p->type_char, off + i));
}
#endif

static boolean maybe_read_brace(parse p) {
    return next_token(p, open_brace)?true:false;
}

static void maybe_skip_comma(parse p) {
    next_token(p, comma);
}

static void skip_to_brace(parse p) {
    for (;;) {
        if (next_token(p, close_brace))
            return;
        if (next_token(p, sym(.))) {
            consume(p);
            expect(p, sym(=));
        }
        consume(p);
        Node *ignore = read_assignment_expr(p);
        if (!ignore)
            return;
        warnt(tok, "excessive initializer: %s", node2s(ignore));
        maybe_skip_comma(p);
    }
}

static void read_initializer_elem(parse p, vector inits, Type *ty, int off, boolean designated);

static void read_array_initializer(parse p, vector inits, Type *ty, int off, boolean designated) {
    boolean has_brace = maybe_read_brace(p);
    boolean flexible = (ty->len <= 0);
    int elemsize = ty->ptr->size;
    int i;
    for (i = 0; flexible || i < ty->len; i++) {
        Token *tok = token(p);
        if (is_keyword(tok, close_brace)) {
            //            if (!has_brace)
                //                unget_token(tok);
            goto finish;
        }
        if ((is_keyword(tok, sym(.)) || is_keyword(tok, open_brace)) && !has_brace && !designated) {
            //            unget_token(tok);
            return;
        }
        if (is_keyword(tok, open_brace)) {
            consume(p);
            int idx = read_intexpr(p);
            if (idx < 0 || (!flexible && ty->len <= idx))
                errort(tok, "array designator exceeds array bounds: %d", idx);
            i = idx;
            expect(p, close_brace);
            designated = true;
            consume(p);
        }
        read_initializer_elem(p, inits, ty->ptr, off + elemsize * i, designated);
        maybe_skip_comma(p);
        designated = false;
    }
    if (has_brace)
        skip_to_brace(p);
 finish:
    if (ty->len < 0) {
        ty->len = i;
        ty->size = elemsize * i;
    }
}

static void read_struct_initializer(parse p, vector inits, Type *ty, int off, boolean designated) {
    boolean has_brace = maybe_read_brace(p);
    // guess the problem here is implicit field names?
    for (;;) {
        Token *tok = token(p);
        if (is_keyword(tok, close_brace)) {
            //            if (!has_brace)
            //                unget_token(tok);
            return;
        }
        buffer fieldname;
        Type *fieldtype;
        if ((is_keyword(tok, sym(.)) || is_keyword(tok, open_brace)) && !has_brace && !designated) {
            //            unget_token(tok);
            return;
        }
        if (is_keyword(tok, sym(.))) {
            tok = token(p);
            if (!tok || tok->kind != sym(ident))
                errort(tok, "malformed desginated initializer: %s", tok2s(tok));
            fieldname = tok->sval;
            // ordered and unoredered
            fieldtype = get(ty->fields, fieldname);
            if (!fieldtype)
                errort(tok, "field does not exist: %s", tok2s(tok));
            designated = true;
            consume(p);
        } else {
            // hmm?
            //            if (i == vector_length(keys))
            //                break;
            //            fieldname = vector_get(keys, i++);
            //            fieldtype = lookup_field(ty, intern(fieldname));
        }
        read_initializer_elem(p, inits, fieldtype, off + fieldtype->offset, designated);
        maybe_skip_comma(p);
        designated = false;
        if (ty->kind != sym(struct))
            break;
    }
    if (has_brace)
        skip_to_brace(p);
}

static void read_initializer_list(parse p,
                                  vector inits, Type *ty, int off, boolean designated) {
    Token *tok = token(p);
    if (is_string(ty)) {
        if (tok->kind == sym(string)) {
            assign_string(p, inits, ty, tok->sval, off);
            return;
        }
        if (is_keyword(tok, open_brace) && token(p)->kind == sym(string)) {
            tok = token(p);
            assign_string(p, inits, ty, tok->sval, off);
            expect(p, close_brace);
            consume(p);
            return;
        }
    }
    // unget_token(tok);
    if (ty->kind == sym(array)) {
        read_array_initializer(p, inits, ty, off, designated);
    } else if (ty->kind == sym(struct)) {
        read_struct_initializer(p, inits, ty, off, designated);
    } else {
        Type *arraytype = make_array_type(ty, 1);
        read_array_initializer(p, inits, arraytype, off, designated);
    }
}

static vector read_decl_init(parse p, Type *ty) {
    vector r = allocate_vector(p->h, 10);
    if (is_keyword(token(p), open_brace) || is_string(ty)) {
        read_initializer_list(p, r, ty, 0, false);
    } else {
        Node *init = conv(read_assignment_expr(p));
        if (is_inttype(init->ty) && init->ty->kind != ty->kind)
            init = ast_conv(ty, init);
        vector_push(r, ast_init(init, ty, 0));
    }
    return r;
}

static Node *read_compound_literal(parse p, Type *ty) {
    buffer name = make_label();
    vector init = read_decl_init(p, ty);
    Node *r = ast_lvar(p, ty, name);
    r->lvarinit = init;
    return r;
}

static Node *read_cast_expr(parse p) {
    Token *tok = token(p);
    // peek is next !
    if (is_keyword(tok, open_paren) && is_type(p, token(p))) {
        Type *ty = read_cast_type(p);
        expect(p, close_paren);
        if (is_keyword(token(p), close_brace)) {
            Node *node = read_compound_literal(p, ty);
            return read_postfix_expr_tail(p, node);
        }
        return ast_uop(sym(cast), ty, read_cast_expr(p));
    }
    //    unget_token(tok);
    return read_unary_expr(p);
}

static Node *read_multiplicative_expr(parse p) {
    Node *node = read_cast_expr(p);
    for (;;) {
        if (next_token(p, sym(*)))      node = binop(p, sym(*), conv(node), conv(read_cast_expr(p)));
        else if (next_token(p, sym(/))) node = binop(p, sym(/), conv(node), conv(read_cast_expr(p)));
        else if (next_token(p, sym(%))) node = binop(p, sym(%), conv(node), conv(read_cast_expr(p)));
        else    return node;
    }
}

static Node *read_additive_expr(parse p) {
    Node *node = read_multiplicative_expr(p);
    for (;;) {
        if      (next_token(p, sym(+))) node = binop(p, sym(+), conv(node), conv(read_multiplicative_expr(p)));
        else if (next_token(p, sym(-))) node = binop(p, sym(-), conv(node), conv(read_multiplicative_expr(p)));
        else    return node;
    }
}

static Node *read_shift_expr(parse p) {
    Node *node = read_additive_expr(p);
    for (;;) {
        symbol op;
        if (next_token(p, sym(<<)))
            op = sym(<<);
        else if (next_token(p, sym(>>)))
            op = sym(>>);
        else
            break;
        Node *right = read_additive_expr(p);
        ensure_inttype(node);
        ensure_inttype(right);
        node = ast_binop(p, node->ty, op, conv(node), conv(right));
    }
    return node;
}

static Node *read_relational_expr(parse p) {
    Node *node = read_shift_expr(p);
    for (;;) {
        if      (next_token(p, sym(<)))   node = binop(p, sym('<'),  conv(node), conv(read_shift_expr(p)));
        else if (next_token(p, sym(>)))   node = binop(p, sym('>'),  conv(read_shift_expr(p)), conv(node));
        else if (next_token(p, sym(<=))) node = binop(p, sym(<=), conv(node), conv(read_shift_expr(p)));
        else if (next_token(p, sym(>=))) node = binop(p, sym(>=), conv(read_shift_expr(p)), conv(node));
        else    return node;
        node->ty = p->type_int;
    }
}

static Node *read_equality_expr(parse p) {
    Node *node = read_relational_expr(p);
    Node *r;
    if (next_token(p, sym(==))) {
        r = binop(p, sym(==), conv(node), conv(read_equality_expr(p)));
    } else if (next_token(p, sym(!=))) {
        r = binop(p, sym(!=), conv(node), conv(read_equality_expr(p)));
    } else {
        return node;
    }
    r->ty = p->type_int;
    return r;
}

static Node *read_bitand_expr(parse p) {
    Node *node = read_equality_expr(p);
    while (next_token(p, sym(&)))
        node = binop(p, sym(&), conv(node), conv(read_equality_expr(p)));
    return node;
}

static Node *read_bitxor_expr(parse p) {
    Node *node = read_bitand_expr(p);
    while (next_token(p,sym(^)))
        node = binop(p, sym(^), conv(node), conv(read_bitand_expr(p)));
    return node;
}

static Node *read_bitor_expr(parse p) {
    Node *node = read_bitxor_expr(p);
    while (next_token(p, sym(|)))
        node = binop(p, sym(|), conv(node), conv(read_bitxor_expr(p)));
    return node;
}

static Node *read_logand_expr(parse p) {
    Node *node = read_bitor_expr(p);
    while (next_token(p, sym(&&)))
        node = ast_binop(p, p->type_int, sym(&&), node, read_bitor_expr(p));
    return node;
}

static Node *read_logor_expr(parse p) {
    Node *node = read_logand_expr(p);
    while (next_token(p, sym(||)))
        node = ast_binop(p, p->type_int, sym(||), node, read_logand_expr(p));
    return node;
}

static Node *read_conditional_expr(parse p);

static Node *do_read_conditional_expr(parse p, Node *cond) {
    Node *then = conv(read_comma_expr(p));
    expect(p, sym(:));
    Node *els = conv(read_conditional_expr(p));
    // [GNU] Omitting the middle operand is allowed.
    Type *t = then ? then->ty : cond->ty;
    Type *u = els->ty;
    // C11 6.5.15p5: if both types are arithemtic type, the result
    // type is the result of the usual arithmetic conversions.
    if (is_inttype(t) && is_inttype(u)) {
        Type *r = usual_arith_conv(p, t, u);
        return ast_ternary(r, cond, (then ? wrap(r, then) : NULL), wrap(r, els));
    }
    return ast_ternary(u, cond, then, els);
}

static Node *read_conditional_expr(parse p) {
    Node *cond = read_logor_expr(p);
    if (!next_token(p, sym(?)))
        return cond;
    return do_read_conditional_expr(p, cond);
}

static Node *read_assignment_expr(parse p) {
    Node *node = read_logor_expr(p);
    Token *tok = token(p);
    if (!tok) return node;
    if (is_keyword(tok, sym(?)))
        return do_read_conditional_expr(p, node);
    symbol cop = get_compound_assign_op(tok);
    if (is_keyword(tok, sym(=)) || cop) {
        Node *value = conv(read_assignment_expr(p));
        if (is_keyword(tok, sym(=)) || cop)
            ensure_lvalue(node);
        Node *right = cop ? binop(p, cop, conv(node), value) : value;
        if (is_inttype(node->ty) && node->ty->kind != right->ty->kind)
            right = ast_conv(node->ty, right);
        return ast_binop(p, node->ty, sym(=), node, right);
    }
    // unget_token(tok);
    return node;
}

static Node *read_struct_field(parse p, Node *struc) {
    // or union?
    if (struc->ty->kind != sym(struct))
        error("struct expected, but got %s", node2s(struc));
    Token *name = token(p);
    if (name->kind != sym(ident))
        error("field name expected, but got %s", tok2s(name));
    Type *field = lookup_field(struc->ty, intern(name->sval));
    if (!field)
        error("struct has no such field: %s", tok2s(name));
    return ast_struct_ref(field, struc, name->sval);
}


static void read_initializer_elem(parse p, vector inits, Type *ty, int off, boolean designated) {
    next_token(p, sym(=));
    if (ty->kind == sym(array) || ty->kind == sym(struct)) {
        read_initializer_list(p, inits, ty, off, designated);
    } else if (next_token(p, open_brace)) {
        read_initializer_elem(p, inits, ty, off, true);
        expect(p, close_brace);
    } else {
        Node *expr = conv(read_assignment_expr(p));
        ensure_assignable(ty, expr->ty);
        vector_push(inits, ast_init(expr, ty, off));
    }
}


static Type *read_func_param(parse p, buffer *name, boolean optional) {
    symbol sclass = 0;
    Type *basety = p->type_int;
    if (is_type(p, token(p))) {
        basety = read_decl_spec(p, &sclass);
    } else if (optional) {
        errort(peek(), "type expected, but got %s", tok2s(peek()));
    }
    Type *ty = read_declarator(p, name, basety, NULL, optional ? DECL_PARAM_TYPEONLY : DECL_PARAM);
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
static void read_declarator_params(parse p, vector types, vector vars, boolean *ellipsis) {
    boolean typeonly = !vars;
    *ellipsis = false;
    for (;;) {
        Token *tok = token(p);
        if (next_token(p, sym(...))) {
            if (vector_length(types) == 0)
                errort(tok, "at least one parameter is required before \"...\"");
            expect(p, close_paren);
            *ellipsis = true;
            return;
        }
        buffer name;
        Type *ty = read_func_param(p, &name, typeonly);
        ensure_not_void(ty);
        vector_push(types, ty);
        if (!typeonly)
            vector_push(vars, ast_lvar(p, ty, name));
        tok = token(p);
        if (is_keyword(tok, close_paren))
            return;
        if (!is_keyword(tok, comma))
            errort(tok, "comma expected, but got %s", tok2s(tok));
    }
}

static Type *read_func_param_list(parse p, vector paramvars, Type *rettype) {
    // C11 6.7.6.3p10: A parameter list with just "void" specifies that
    // the function has no parameters.
    Token *tok = token(p);
    if (is_keyword(tok, sym(void)) && next_token(p, close_paren))
        return make_func_type(rettype, allocate_vector(p->h, 10), false);

    // C11 6.7.6.3p14: K&R-style un-prototyped declaration or
    // function definition having no parameters.
    // We return a type representing K&R-style declaration here.
    // If this is actually part of a declartion, the type will be fixed later.
    if (is_keyword(tok, close_paren))
        return make_func_type(rettype, allocate_vector(p->h, 10), true);
    //    unget_token(tok);

    consume(p);
    if (next_token(p, sym(...)))
        errort(tok2, "at least one parameter is required before \"...\"");
    if (is_type(p, token(p))) {
        boolean ellipsis;
        vector paramtypes = allocate_vector(p->h, 10);
        read_declarator_params(p, paramtypes, paramvars, &ellipsis);
        return make_func_type(rettype, paramtypes, ellipsis);
    }
    if (!paramvars)
        errort(tok, "invalid function definition");
    vector paramtypes = allocate_vector(p->h, 10);
    for (int i = 0; i < vector_length(paramvars); i++)
        vector_push(paramtypes, p->type_int);
    return make_func_type(rettype, paramtypes, false);
}

static Type *read_declarator_tail(parse p, Type *basety, vector params);

static Type *read_declarator_array(parse p, Type *basety) {
    int len;
    if (next_token(p, close_bracket)) {
        len = -1;
    } else {
        len = read_intexpr(p);
        expect(p, close_brace);
    }
    consume(p);
    Type *t = read_declarator_tail(p, basety, NULL);
    if (t->kind == sym(func))
        errort(tok, "array of functions");
    return make_array_type(t, len);
}

static Type *read_declarator_func(parse p, Type *basety, vector param) {
    if (basety->kind == sym(func))
        error("function returning a function");
    if (basety->kind == sym(array))
        error("function returning an array");
    return read_func_param_list(p, param, basety);
}

static Type *read_declarator_tail(parse p, Type *basety, vector params) {
    if (next_token(p, open_brace))
        return read_declarator_array(p, basety);
    if (next_token(p, open_paren))
        return read_declarator_func(p, basety, params);
    return basety;
}

static void skip_type_qualifiers(parse p) {
    while (next_token(p, sym(const)) || next_token(p, sym(volatile)) || next_token(p, sym(RESTRICT)));
}

// C11 6.7.6: Declarators
static Type *read_declarator(parse p, buffer *rname, Type *basety, vector params, int ctx) {
    if (next_token(p, open_paren)) {
        // open_paren is either beginning of grouping parentheses or of a function parameter list.
        // If the next token is a type name, a parameter list must follow.
        if (is_type(p, token(p)))
            return read_declarator_func(p, basety, params);
        // If not, it's grouping. In that case we have to read from outside.
        // For example, consider int (*)(), which is "pointer to function returning int".
        // We have only read "int" so far. We don't want to pass "int" to
        // a recursive call, or otherwise we would get "pointer to int".
        // Here, we pass a dummy object to get "pointer to <something>" first,
        // continue reading to get "function returning int", and then combine them.
        Type *stub = make_stub_type();
        Type *t = read_declarator(p, rname, stub, params, ctx);
        expect(p, close_paren);
        *stub = *read_declarator_tail(p, basety, params);
        return t;
    }
    if (next_token(p, sym(*))) {
        skip_type_qualifiers(p);
        return read_declarator(p, rname, make_ptr_type(basety), params, ctx);
    }
    Token *tok = token(p);
    if (tok->kind == sym(ident)) {
        if (ctx == DECL_CAST)
            errort(tok, "identifier is not expected, but got %s", tok2s(tok));
        *rname = tok->sval;
        return read_declarator_tail(p, basety, params);
    }
    if (ctx == DECL_BODY || ctx == DECL_PARAM)
        errort(tok, "identifier, ( or * are expected, but got %s", tok2s(tok));
    //    unget_token(tok);
    return read_declarator_tail(p, basety, params);
}

static void read_static_local_var(parse p, Type *ty, buffer name) {
    Node *var = ast_static_lvar(p, ty, name);
    vector init = NULL;
    if (next_token(p, sym(=))) {
        // hmm?
        tuple orig = p->localenv;
        p->localenv = NULL;
        init = read_decl_init(p, ty);
        p->localenv = orig;
    }
    vector_push(p->toplevels, ast_decl(var, init));
}

static void read_decl(parse p, vector block, boolean isglobal) {
    symbol sclass = 0;
    Type *basetype = read_decl_spec(p, &sclass);
    if (next_token(p, semicolon))
        return;
    for (;;) {
        buffer name = NULL;
        Type *ty = read_declarator(p, &name, copy_incomplete_type(basetype), NULL, DECL_BODY);
        ty->isstatic = (sclass == sym(static));
        if (sclass == sym(typedef)) {
            Node *r = make_ast(&(Node){ sym(typedef), ty }, 0);
            set(p->globalenv, name, r);            
        } else if (ty->isstatic && !isglobal) {
            ensure_not_void(ty);
            read_static_local_var(p, ty, name);
        } else {
            ensure_not_void(ty);
            Node *var = (isglobal ? ast_gvar : ast_lvar)(p, ty, name);
            if (next_token(p, sym(=))) {
                vector_push(block, ast_decl(var, read_decl_init(p, ty)));
            } else if (sclass != sym(extern) && ty->kind != sym(func)) {
                vector_push(block, ast_decl(var, NULL));
            }
        }
        if (next_token(p, semicolon))
            return;
        if (!next_token(p, colon))
            errort(peek(), "';' or ',' are expected, but got %s", tok2s(peek()));
    }
}

tuple make_map_parent(tuple p)
{
}

static Node *read_func_body(parse p, Type *functype, buffer fname, vector params) {
    p->localenv = make_map_parent(p->localenv);
    p->localvars = allocate_vector(p->h, 10);
    p->current_func_type = functype;
    Node *funcname = ast_string(p, fname);
    set(p->localenv, "__func__", funcname);
    set(p->localenv, "__FUNCTION__", funcname);
    Node *body = read_compound_stmt(p);
    Node *r = ast_func(functype, fname, params, body, p->localvars);
    p->current_func_type = NULL;
    p->localenv = NULL;
    p->localvars = NULL;
    return r;
}

static void skip_parentheses(parse p, vector buf) {
    for (;;) {
        Token *tok = token(p);
        if (tok->kind == sym(eof))
            error("premature end of input");
        vector_push(buf, tok);
        if (is_keyword(tok, close_paren))
            return;
        if (is_keyword(tok, open_paren))
            skip_parentheses(p, buf);
    }
}

// is_funcdef returns true if we are at beginning of a function definition.
// The basic idea is that if we see '{' or a type keyword after a closing
// parenthesis of a function parameter list, we were reading a function
// definition. (Usually '{' comes after a closing parenthesis.
// A type keyword is allowed for K&R-style function definitions.)
static boolean is_funcdef(parse p) {
    vector buf = allocate_vector(p->h, 10);
    boolean r = false;
    for (;;) {
        Token *tok = token(p);
        vector_push(buf, tok);
        if (tok->kind == sym(eof))
            error("premature end of input");
        if (is_keyword(tok, semicolon))
            break;
        if (is_type(p, tok))
            continue;
        if (is_keyword(tok, open_paren)) {
            skip_parentheses(p, buf);
            continue;
        }
        if (tok->kind != sym(ident))
            continue;
        if (!is_keyword(token(p), open_paren))
            continue;
        vector_push(buf, token(p));
        skip_parentheses(p, buf);
        r = (is_keyword(token(p), open_brace) || is_type(p, token(p)));
        break;
    }
    //    while (vector_len(buf) > 0)
    //        unget_token(vector_pop(buf));
    return r;
}

static void backfill_labels(parse p) {
    for (int i = 0; i < vector_length(p->gotos); i++) {
        Node *src = vector_get(p->gotos, i);
        buffer label = src->label;
        Node *dst = get(p->labels, label);
        if (!dst)
            error("stray %s: %s", src->kind == sym(goto) ? "goto" : "unary &&", label);
        if (dst->newlabel)
            src->newlabel = dst->newlabel;
        else
            src->newlabel = dst->newlabel = make_label();
    }
}

static Node *read_funcdef(parse p) {
    symbol sclass = 0;
    Type *basetype = read_decl_spec(p, &sclass);
    p->localenv = make_map_parent(p->globalenv);
    p->gotos = allocate_vector(p->h, 10);
    //    p->labels = timm();
    buffer name;
    vector params = allocate_vector(p->h, 10);
    Type *functype = read_declarator(p, &name, basetype, params, DECL_BODY);
    functype->isstatic = (sclass == sym(static));
    ast_gvar(p, functype, name);
    expect(p, open_brace);
    Node *r = read_func_body(p, functype, name, params);
    backfill_labels(p);
    p->localenv = NULL;
    return r;
}

static Node *read_boolean_expr(parse p) {
    Node *cond = read_expr(p);
    return cond;
}

static Node *read_if_stmt(parse p) {
    expect(p, open_paren);
    Node *cond = read_boolean_expr(p);
    expect(p, close_paren);
    Node *then = read_stmt(p);
    if (!next_token(p, sym(else)))
        return ast_if(cond, then, NULL);
    Node *els = read_stmt(p);
    return ast_if(cond, then, els);
}

static void read_decl_or_stmt(parse p, vector list) {
    Token *tok = token(p);
    if (tok->kind == sym(eof))
        error("premature end of input");
    // mark_location();
    if (is_type(p, tok)) {
        read_decl(p, list, false);
    } else {
        Node *stmt = read_stmt(p);
        if (stmt)
            vector_push(list, stmt);
    }
}

static Node *read_opt_decl_or_stmt(parse p) {
    if (next_token(p, semicolon))
        return NULL;
    vector list = allocate_vector(p->h, 10);
    read_decl_or_stmt(p, list);
    return ast_compound_stmt(list);
}

static void push_scope(parse p)
{
}

static void pop_scope(parse p)
{
}


static Node *read_for_stmt(parse p) {
    expect(p, open_paren);
    buffer beg = make_label();
    buffer mid = make_label();
    buffer end = make_label();
    push_scope(p);    
    Node *init = read_opt_decl_or_stmt(p);
    Node *cond = read_comma_expr(p);
    expect(p, semicolon);
    Node *step = read_comma_expr(p);
    expect(p, close_paren);
    Node *body = read_stmt(p);
    pop_scope(p);

    vector v = allocate_vector(p->h, 10);
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
    expect(p, open_paren);
    Node *cond = read_boolean_expr(p);
    expect(p, close_paren);

    buffer beg = make_label();
    buffer end = make_label();
    push_scope(p);
    //     SET_JUMP_LABELS(beg, end);
    Node *body = read_stmt(p);
    pop_scope(p);    

    vector v = allocate_vector(p->h, 10);
    vector_push(v, ast_dest(beg));
    vector_push(v, ast_if(cond, body, ast_jump(end)));
    vector_push(v, ast_jump(beg));
    vector_push(v, ast_dest(end));
    return ast_compound_stmt(v);
}

static Node *read_do_stmt(parse p) {
    buffer beg = make_label();
    buffer end = make_label();
    push_scope(p);
    Node *body = read_stmt(p);
    pop_scope(p);
    Token *tok = token(p);
    if (!is_keyword(tok, sym(while)))
        errort(tok, "'while' is expected, but got %s", tok2s(tok));
    expect(p, open_paren);
    Node *cond = read_boolean_expr(p);
    expect(p, close_paren);
    expect(p, semicolon);

    vector v = allocate_vector(p->h, 10);
    vector_push(v, ast_dest(beg));
    if (body)
        vector_push(v, body);
    vector_push(v, ast_if(cond, ast_jump(beg), NULL));
    vector_push(v, ast_dest(end));
    return ast_compound_stmt(v);
}

static Node *make_switch_jump(parse p, Node *var, Case *c) {
    Node *cond;
    if (c->beg == c->end) {
        cond = ast_binop(p, p->type_int, sym(=), var, ast_inttype(p, p->type_int, c->beg));
    } else {
        // [GNU] case i ... j is compiled to if (i <= cond && cond <= j) goto <label>.
        Node *x = ast_binop(p, p->type_int, sym(<=), ast_inttype(p, p->type_int, c->beg), var);
        Node *y = ast_binop(p, p->type_int, sym(<=), var, ast_inttype(p, p->type_int, c->end));
        cond = ast_binop(p, p->type_int, sym(logand), x, y);
    }
    return ast_if(cond, ast_jump(c->label), NULL);
}

// C11 6.8.4.2p3: No two case constant expressions have the same value.
static void check_case_duplicates(vector cases) {
    int len = vector_length(cases);
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
static Node *read_switch_stmt(parse p)
{
    expect(p, open_paren);
    Node *expr = conv(read_expr(p));
    ensure_inttype(expr);
    expect(p, close_paren);

    buffer end = make_label();
    //    SET_SWITCH_CONTEXT(p, end);
    push_scope(p);
    Node *body = read_stmt(p);
    vector v = allocate_vector(p->h, 10);
    Node *var = ast_lvar(p, expr->ty, make_tempname());
    vector_push(v, ast_binop(p, expr->ty, sym(=), var, expr));
    for (int i = 0; i < vector_length(p->cases); i++)
        vector_push(v, make_switch_jump(p, var, vector_get(p->cases, i)));
    vector_push(v, ast_jump(p->defaultcase ? p->defaultcase : end));
    if (body)
        vector_push(v, body);
    vector_push(v, ast_dest(end));
    pop_scope(p);    
    return ast_compound_stmt(v);
}

static Node *read_label_tail(parse p, Node *label) {
    Node *stmt = read_stmt(p);
    vector v = allocate_vector(p->h, 10);
    vector_push(v, label);
    if (stmt)
        vector_push(v, stmt);
    return ast_compound_stmt(v);
}

static Node *read_case_label(parse p, Token *tok) {
    if (!p->cases)
        errort(tok, "stray case label");
    buffer label = make_label();
    int beg = read_intexpr(p);
    if (next_token(p, sym(...))) {
        int end = read_intexpr(p);
        expect(p, colon);
        if (beg > end)
            errort(tok, "case region is not in correct order: %d ... %d", beg, end);
        vector_push(p->cases, make_case(beg, end, label));
    } else {
        expect(p, colon);
        vector_push(p->cases, make_case(beg, beg, label));
    }
    check_case_duplicates(p->cases);
    return read_label_tail(p, ast_dest(label));
}

static Node *read_default_label(parse p, Token *tok) {
    expect(p, colon);
    if (p->defaultcase)
        errort(tok, "duplicate default");
    p->defaultcase = make_label();
    return read_label_tail(p, ast_dest(p->defaultcase));
}

static Node *read_break_stmt(parse p, Token *tok) {
    expect(p, semicolon);
    if (!p->lbreak)
        errort(tok, "stray break statement");
    return ast_jump(p->lbreak);
}

static Node *read_continue_stmt(parse p, Token *tok) {
    expect(p, semicolon);
    if (!p->lcontinue)
        errort(tok, "stray continue statement");
    return ast_jump(p->lcontinue);
}

static Node *read_return_stmt(parse p) {
    Node *retval = read_comma_expr(p);
    expect(p, semicolon);
    if (retval)
        return ast_return(ast_conv(p->current_func_type->rettype, retval));
    return ast_return(NULL);
}

static Node *read_goto_stmt(parse p) {
    if (next_token(p, sym(*))) {
        // [GNU] computed goto. "goto *p" jumps to the address pointed by p.
        consume(p);
        Node *expr = read_cast_expr(p);
        if (expr->ty->kind != sym(ptr))
            errort(tok, "pointer expected for computed goto, but got %s", node2s(expr));
        return ast_computed_goto(expr);
    }
    Token *tok = token(p);
    if (!tok || tok->kind != sym(ident))
        errort(tok, "identifier expected, but got %s", tok2s(tok));
    expect(p, semicolon);
    Node *r = ast_goto(tok->sval);
    vector_push(p->gotos, r);
    return r;
}

static Node *read_label(parse p, Token *tok)
{
    buffer label = tok->sval;
    if (get(p->labels, label))
        errort(tok, "duplicate label: %s", tok2s(tok));
    Node *r = ast_label(label);
    set(p->labels, label, r);
    return read_label_tail(p, r);
}

static Node *read_stmt(parse p) {
    Token *tok = token(p);
    if (tok->kind == sym(keyword)) {
        if (tok->id == open_brace) read_compound_stmt(p);
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
    if ((tok->kind == sym(ident)) && next_token(p, colon))
        return read_label(p, tok);
    
    //    unget_token(tok);
    Node *r = read_comma_expr(p);
    expect(p, semicolon);
    return r;
}

static Node *read_compound_stmt(parse p) {
    push_scope(p);
    vector list = allocate_vector(p->h, 10);
    for (;;) {
        if (next_token(p, close_brace))
            break;
        read_decl_or_stmt(p, list);
    }
    pop_scope(p);
    return ast_compound_stmt(list);
}


vector read_toplevels(parse p) {
    p->toplevels = allocate_vector(p->h, 10);
    for (;;) {
        if (token(p)->kind == sym(eof))
            return p->toplevels;
        if (is_funcdef(p))
            vector_push(p->toplevels, read_funcdef(p));
        else
            read_decl(p, p->toplevels, true);
    }
}

// C11 5.1.1.2p6 Adjacent string literal tokens are concatenated.
static void concatenate_string(parse p, Token *tok) {
    buffer b = allocate_buffer(p->h, 10);
    push_buffer(b, tok->sval);
    while (token(p)->kind == sym(string)) {
        Token *tok2 = token(p);
        push_buffer(b, tok2->sval);
    }
    tok->sval = b;
}

static Token *token_get(parse p) {
    Token *r = token(p);
    if (r->kind == sym(invalid))
        errort(r, "stray character in program: '%c'", r->c);
    if (r->kind == sym(string) && token(p)->kind == sym(string))
        concatenate_string(p, r);
    return r;
}

static void define_builtin(parse p, buffer name, Type *rettype, vector paramtypes) {
    ast_gvar(p, make_func_type(rettype, paramtypes, false), name);
}

parse parse_init(heap h) {
    parse p = allocate(h, sizeof(struct parse));
    p->type_void = &(Type){ sym(void), 0, 0, false };    
    Type *v = make_ptr_type(p->type_void);
    vector voidptr = build_vector(h, v);
    vector two_voidptrs = build_vector(h, v, v);

    p->type_bool = &(Type){ sym(boolean), 1, 1, true };
    p->type_char = &(Type){ sym(char), 1, 1, false };
    p->type_short = &(Type){ sym(short), 2, 2, false };
    p->type_int = &(Type){ sym(int), 4, 4, false };
    p->type_long = &(Type){ sym(long), 8, 8, false };
    p->type_llong = &(Type){ sym(llong), 8, 8, false };
    p->type_uchar = &(Type){ sym(char), 1, 1, true };
    p->type_ushort = &(Type){ sym(short), 2, 2, true };
    p->type_uint = &(Type){ sym(int), 4, 4, true };
    p->type_ulong = &(Type){ sym(long), 8, 8, true };
    p->type_ullong = &(Type){ sym(llong), 8, 8, true };
    p->type_enum = &(Type){ sym(enum), 4, 4, false };
    open_paren = intern(staticbuffer("("));
    close_paren = intern(staticbuffer(")"));
    open_brace = intern(staticbuffer("{"));        
    close_brace = intern(staticbuffer("}"));
    open_bracket = intern(staticbuffer("["));
    close_bracket = intern(staticbuffer("]"));    
    comma = intern(staticbuffer(","));
    semicolon = intern(staticbuffer(";"));
    colon = intern(staticbuffer(":"));

    define_builtin(p, staticbuffer("__builtin_return_address"), v, voidptr);
    define_builtin(p, staticbuffer("__builtin_reg_class"), p->type_int, voidptr);
    define_builtin(p, staticbuffer("__builtin_va_arg"), p->type_void, two_voidptrs);
    define_builtin(p, staticbuffer("__builtin_va_start"), p->type_void, voidptr);        
}
