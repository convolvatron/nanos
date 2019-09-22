// Copyright 2012 Rui Ueyama. Released under the MIT license.
#include "8cc.h"

static tuple *globalenv = 0;
static tuple *localenv;
static tuple *tags = 0;
static tuple *labels;

static vector *toplevels;
static vector localvars;
static vector gotos;
static vector cases;
static Type *current_func_type;

static char *defaultcase;
static char *lbreak;
static char *lcontinue;

Type *type_void *type_bool, *type_char, *type_short, *type_int,
    *type_long, *type_llong, *type_uchar, *type_ushort, *type_uint,
    *type_ulong, *type_ullong, *type_enum;

static Type* make_ptr_type(Type *ty);
static Type* make_array_type(Type *ty, int size);
static Node *read_compound_stmt(void);
static void read_decl_or_stmt(vector list);
static Node *conv(Node *node);
static Node *read_stmt(void);
static boolean is_type(Token *tok);
static Node *read_unary_expr(void);
static void read_decl(vector toplevel, boolean isglobal);
static Type *read_declarator_tail(Type *basetype, vector params);
static Type *read_declarator(char **name, Type *basetype, vector params, int ctx);
static Type *read_abstract_declarator(Type *basetype);
static Type *read_decl_spec(int *sclass);
static Node *read_struct_field(Node *struc);
static void read_initializer_list(vector inits, Type *ty, int off, boolean designated);
static Type *read_cast_type(void);
static vector read_decl_init(Type *ty);
static Node *read_booleanean_expr(void);
static Node *read_expr_opt(void);
static Node *read_conditional_expr(void);
static Node *read_assignment_expr(void);
static Node *read_cast_expr(void);
static Node *read_comma_expr(void);
static Token *get_token(void);
static Token *peek(void);

typedef struct {
    int beg;
    int end;
    char *label;
} Case;

/*
 * Constructors
 */

string make_tempname() {
    static int c = 0;
    return aprintf(transient, ".T%d", c++);
}

string make_label() {
    static int c = 0;
    return aprintf(transient, ".L%d", c++);
}

static string make_static_label(char *name) {
    static int c = 0;
    return aprintf(transient, ".S%d.%s", c++, name);
}

static Case *make_case(int beg, int end, char *label) {
    Case *r = allocate(transient, sizeof(Case));
    r->beg = beg;
    r->end = end;
    r->label = label;
    return r;
}

static Node *make_ast(Node *tmpl) {
    Node *r = allocate(transient, sizeof(Node));
    *r = *tmpl;
    r->sourceLoc = source_loc;
    return r;
}

static Node *ast_uop(symbol kind, Type *ty, Node *operand) {
    return make_ast(&(Node){ kind, ty, .operand = operand });
}

static Node *ast_binop(Type *ty, symbol kind, Node *left, Node *right) {
    Node *r = make_ast(&(Node){ kind, ty });
    r->left = left;
    r->right = right;
    return r;
}

static Node *ast_inttype(Type *ty, long val) {
    return make_ast(&(Node){ sym(literal), ty, .ival = val });
}

static Node *ast_floattype(Type *ty, double val) {
    return make_ast(&(Node){ sym(literal), ty, .fval = val });
}

// aren't these the same?
static Node *ast_lvar(tuple env, Type *ty, char *name) {
    Node *r = make_ast(&(Node){ sym(lvar), ty, .varname = name });
    if (localenv)
        set(env, name, r);
    if (localvars)
        vector_push(localvars, r);
    return r;
}

static Node *ast_gvar(tuple env, Type *ty, char *name) {
    Node *r = make_ast(&(Node){ sym(gvar), ty, .varname = name, .glabel = name });
    set(env, name, r);
    return r;
}

static Node *ast_static_lvar(Type *ty, char *name) {
    Node *r = make_ast(&(Node){
            .kind = sym(gvar),
                .ty = ty,
                .varname = name,
                .glabel = make_static_label(name) });
    assert(localenv);
    set(localenv, name, r);
    return r;
}

static Node *ast_typedef(tuple env, Type *ty, char *name) {
    Node *r = make_ast(&(Node){ sym(typedef), ty });
    set(env, name, r);
    return r;
}

static Node *ast_string(int enc, buffer in)
{
    Type *ty;
    buffer b = in;
    ty = make_array_type(type_char, buffer_length(in));
    return make_ast(&(Node){ sym(literal), .ty = ty, .sval = b });
}

static Node *ast_funcall(Type *ftype, buffer fname, vector args) {
    return make_ast(&(Node){
            .kind = sym(funcall),
                .ty = ftype->rettype,
                .fname = fname,
                .args = args,
                .ftype = ftype });
}

static Node *ast_funcdesg(Type *ty, string fname) {
    return make_ast(&(Node){ sym(funcdesg), ty, .fname = fname });
}

static Node *ast_funcptr_call(Node *fptr, vector args) {
    assert(fptr->ty->kind == sym(ptr));
    assert(fptr->ty->ptr->kind == sym(func));
    return make_ast(&(Node){
            .kind = sym(funcptr_call),
                .ty = fptr->ty->ptr->rettype,
                .fptr = fptr,
                .args = args });
}

static Node *ast_func(Type *ty, string fname, vector params, Node *body, vector localvars) {
    return make_ast(&(Node){
            .kind = sym(func),
                .ty = ty,
                .fname = fname,
                .params = params,
                .localvars = localvars,
                .body = body});
}

static Node *ast_decl(Node *var, vector init) {
    return make_ast(&(Node){ sym(decl), .declvar = var, .declinit = init });
}

static Node *ast_init(Node *val, Type *totype, int off) {
    return make_ast(&(Node){ sym(init), .initval = val, .initoff = off, .totype = totype });
}

static Node *ast_conv(Type *totype, Node *val) {
    return make_ast(&(Node){ sym(conv), totype, .operand = val });
}

static Node *ast_if(Node *cond, Node *then, Node *els) {
    return make_ast(&(Node){ sym(if), .cond = cond, .then = then, .els = els });
}

static Node *ast_ternary(Type *ty, Node *cond, Node *then, Node *els) {
    return make_ast(&(Node){ sym(ternary), ty, .cond = cond, .then = then, .els = els });
}

static Node *ast_return(Node *retval) {
    return make_ast(&(Node){ sym(return), .retval = retval });
}

static Node *ast_compound_stmt(vector stmts) {
    return make_ast(&(Node){ sym(compound_stmt), .stmts = stmts });
}

static Node *ast_struct_ref(Type *ty, Node *struc, char *name) {
    return make_ast(&(Node){ sym(struct_ref), ty, .struc = struc, .field = name });
}

static Node *ast_goto(char *label) {
    return make_ast(&(Node){ sym(goto), .label = label });
}

static Node *ast_jump(char *label) {
    return make_ast(&(Node){ sym(goto), .label = label, .newlabel = label });
}

static Node *ast_computed_goto(Node *expr) {
    return make_ast(&(Node){ sym(computed_goto), .operand = expr });
}

static Node *ast_label(char *label) {
    return make_ast(&(Node){ sym(label), .label = label });
}

static Node *ast_dest(char *label) {
    return make_ast(&(Node){ sym(label), .label = label, .newlabel = label });
}

static Node *ast_label_addr(char *label) {
    return make_ast(&(Node){ sym(label_addr), make_ptr_type(type_void), .label = label });
}

static Type *make_type(Type *tmpl) {
    Type *r = allocate(transient, sizeof(Type));
    *r = *tmpl;
    return r;
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

static Type* make_ptr_type(Type *ty) {
    return make_type(&(Type){ sym(ptr), .ptr = ty, .size = 8, .align = 8 });
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

static boolean is_string(Type *ty) {
    return ty->kind == sym(array) && ty->ptr->kind == sym(char);
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

static void expect(char id) {
    Token *tok = get_token();
    if (!is_keyword(tok, id))
        errort(tok, "'%c' expected, but got %s", id, string_from_token(transient, tok));
}

static Type *copy_incomplete_type(Type *ty) {
    if (!ty) return 0;
    return (ty->len == -1) ? copy_type(ty) : ty;
}

static Type *get_typedef(tuple env, buffer name) {
    Node *node = get(env, intern(name));
    return (node && node->kind) == sym(typedef) ? node->ty : NULL;
}

static boolean is_type(tuple env, Token *tok)
{
    if (tok->kind == sym(ident))
        return get_typedef(tok->sval);
    if (tok->kind != sym(keyword))
        return false;
    switch (tok->id) {
        // all the standard types were pulled in with redefining op
    default:
        return false;
    }
}

static boolean next_token(symbol kind) {
    Token *tok = get_token();
    if (is_keyword(tok, kind))
        return true;
    unget_token(tok);
    return false;
}

void *make_pair(heap h, void *first, void *second) {
    void **r = allocate(h, sizeof(void *) * 2);
    r[0] = first;
    r[1] = second;
    return r;
}

/*
 * type conversion
 */

static Node *conv(Node *node) {
    // wtf?
    if (!node)
        return NULL;
    type *ty = node->ty;
    switch (ty->kind) {
    case sym(array):
        // c11 6.3.2.1p3: an array of t is converted to a pointer to t.
        return ast_uop(sym(conv), make_ptr_type(ty->ptr), node);
    case sym(func):
        // c11 6.3.2.1p4: a function designator is converted to a pointer to the function.
        return ast_uop(sym(addr), make_ptr_type(ty), node);
    case sym(short): case sym(char): case sym(boolean):
        // c11 6.3.1.1p2: the integer promotions
        return ast_conv(type_int, node);
    case sym(int):
        if (ty->bitsize > 0)
            return ast_conv(type_int, node);
    }
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
static Type *usual_arith_conv(Type *t, Type *u) {
    assert(is_arithtype(t));
    assert(is_arithtype(u));
    // umm .. uh oh
    if (t->kind < u->kind) {
        // Make t the larger type
        Type *tmp = t;
        t = u;
        u = tmp;
    }
    assert(is_inttype(t) && t->size >= type_int->size);
    assert(is_inttype(u) && u->size >= type_int->size);
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
        return true
            return false;
}

static Node *binop(int op, Node *lhs, Node *rhs) {
    
    if (lhs->ty->kind == sym(ptr) && rhs->ty->kind == sym(ptr)) {
        if (!valid_pointer_binop(op))
            error("invalid pointer arith");
        // C11 6.5.6.9: Pointer subtractions have type ptrdiff_t.
        if (op == '-')
            return ast_binop(type_long, op, lhs, rhs);
        // C11 6.5.8.6, 6.5.9.3: Pointer comparisons have type int.
        return ast_binop(type_int, op, lhs, rhs);
    }
    if (lhs->ty->kind == sym(ptr))
        return ast_binop(lhs->ty, op, lhs, rhs);
    if (rhs->ty->kind == sym(ptr))
        return ast_binop(rhs->ty, op, rhs, lhs);
    assert(is_arithtype(lhs->ty));
    assert(is_arithtype(rhs->ty));
    Type *r = usual_arith_conv(lhs->ty, rhs->ty);
    return ast_binop(r, op, wrap(r, lhs), wrap(r, rhs));
}

static boolean is_same_struct(Type *a, Type *b) {
    if (a->kind != b->kind)
        return false;
    switch (a->kind) {
    case sym(array):
        return a->len == b->len &&
            is_same_struct(a->ptr, b->ptr);
    case sym(ptr):
        return is_same_struct(a->ptr, b->ptr);
    case sym(struct): {
        if (a->is_struct != b->is_struct)
            return false;
        vector ka = dict_keys(a->fields);
        vector kb = dict_keys(b->fields);
        if (vector_length(ka) != vector_length(kb))
            return false;
        for (int i = 0; i < vec_len(ka); i++)
            if (!is_same_struct(vector_get(ka, i), vector_get(kb, i)))
                return false;
        return true;
    }
    default:
        return true;
    }
}

static void ensure_assignable(Type *totype, Type *fromtype) {
    if ((is_arithtype(totype) || totype->kind == sym(ptr)) &&
        (is_arithtype(fromtype) || fromtype->kind == sym(ptr)))
        return;
    if (is_same_struct(totype, fromtype))
        return;
    error("incompatible kind: <%s> <%s>", ty2s(totype), ty2s(fromtype));
}

/*
 * Numeric literal
 */

static Type *read_int_suffix(char *s) {
    if (!strcasecmp(s, "u"))
        return type_uint;
    if (!strcasecmp(s, "l"))
        return type_long;
    if (!strcasecmp(s, "ul") || !strcasecmp(s, "lu"))
        return type_ulong;
    if (!strcasecmp(s, "ll"))
        return type_llong;
    if (!strcasecmp(s, "ull") || !strcasecmp(s, "llu"))
        return type_ullong;
    return NULL;
}

static Node *read_int(Token *tok) {
    char *s = tok->sval;
    char *end;
    long v = !strncasecmp(s, "0b", 2)
        ? strtoul(s + 2, &end, 2) : strtoul(s, &end, 0);
    Type *ty = read_int_suffix(end);
    if (ty)
        return ast_inttype(ty, v);
    if (*end != '\0')
        errort(tok, "invalid character '%c': %s", *end, s);

    // C11 6.4.4.1p5: Decimal constant type is int, long, or long long.
    // In 8cc, long and long long are the same size.
    boolean base10 = (*s != '0');
    if (base10) {
        ty = !(v & ~(long)INT_MAX) ? type_int : type_long;
        return ast_inttype(ty, v);
    }
    // Octal or hexadecimal constant type may be unsigned.
    ty = !(v & ~(unsigned long)INT_MAX) ? type_int
        : !(v & ~(unsigned long)UINT_MAX) ? type_uint
        : !(v & ~(unsigned long)LONG_MAX) ? type_long
        : type_ulong;
    return ast_inttype(ty, v);
}

static Node *read_float(Token *tok) {
    char *s = tok->sval;
    char *end;
    double v = strtod(s, &end);
    // C11 6.4.4.2p4: The default type for flonum is double.
    if (!strcasecmp(end, "l"))
        return ast_floattype(type_ldouble, v);
    if (!strcasecmp(end, "f"))
        return ast_floattype(type_float, v);
    if (*end != '\0')
        errort(tok, "invalid character '%c': %s", *end, s);
    return ast_floattype(type_double, v);
}

static Node *read_number(Token *tok) {
    char *s = tok->sval;
    boolean isfloat = strpbrk(s, ".pP") || (strncasecmp(s, "0x", 2) && strpbrk(s, "eE"));
    return isfloat ? read_float(tok) : read_int(tok);
}

/*
 * Sizeof operator
 */

static Type *read_sizeof_operand_sub() {
    Token *tok = get();
    if (is_keyword(tok, '(') && is_type(peek())) {
        Type *r = read_cast_type();
        expect(')');
        return r;
    }
    unget_token(tok);
    return read_unary_expr()->ty;
}

static Node *read_sizeof_operand() {
    Type *ty = read_sizeof_operand_sub();
    // Sizeof on void or function type is GNU extension
    int size = (ty->kind == sym(void) || ty->kind == sym(func)) ? 1 : ty->size;
    assert(0 <= size);
    return ast_inttype(type_ulong, size);
}

/*
 * Alignof operator
 */

static Node *read_alignof_operand() {
    expect('(');
    Type *ty = read_cast_type();
    expect(')');
    return ast_inttype(type_ulong, ty->align);
}

/*
 * Function arguments
 */

static vector read_func_args(parser p,  vector params) {
    vector args = allocate_vector(p->h);
    int i = 0;
    for (;;) {
        if (next_token(')')) break;
        Node *arg = conv(read_assignment_expr());
        Type *paramtype;
        if (i < vec_len(params)) {
            paramtype = vec_get(params, i++);
        } else {
            paramtype = 
                is_inttype(arg->ty) ? type_int :
                arg->ty;
        }
        ensure_assignable(paramtype, arg->ty);
        if (paramtype->kind != arg->ty->kind)
            arg = ast_conv(paramtype, arg);
        vector_push(args, arg);
        Token *tok = get();
        if (is_keyword(tok, ')')) break;
        if (!is_keyword(tok, ','))
            errort(tok, "unexpected token: '%s'", tok2s(tok));
    }
    return args;
}

static Node *read_funcall(Node *fp) {
    if (fp->kind == sym(addr) && fp->operand->kind == sym(funcdesg)) {
        Node *desg = fp->operand;
        vector args = read_func_args(desg->ty->params);
        return ast_funcall(desg->ty, desg->fname, args);
    }
    vector args = read_func_args(fp->ty->ptr->params);
    return ast_funcptr_call(fp, args);
}

/*
 * _Generic
 */

static boolean type_compatible(Type *a, Type *b) {
    if (a->kind == sym(struct))
        return is_same_struct(a, b);
    if (a->kind != b->kind)
        return false;
    if (a->ptr && b->ptr)
        return type_compatible(a->ptr, b->ptr);
    if (is_arithtype(a) && is_arithtype(b))
        return same_arith_type(a, b);
    return true;
}

static vector read_generic_list(Node **defaultexpr) {
    vector r = allocate_vector(h);
    for (;;) {
        if (next_token(')'))
            return r;
        Token *tok = peek();
        if (next_token(KDEFAULT)) {
            if (*defaultexpr)
                errort(tok, "default expression specified twice");
            expect(':');
            *defaultexpr = read_assignment_expr();
        } else {
            Type *ty = read_cast_type();
            expect(':');
            Node *expr = read_assignment_expr();
            vector_push(r, make_pair(ty, expr));
        }
        next_token(',');
    }
}

static Node *read_generic() {
    expect('(');
    Token *tok = peek();
    Node *contexpr = read_assignment_expr();
    expect(',');
    Node *defaultexpr = NULL;
    vector list = read_generic_list(&defaultexpr);
    for (int i = 0; i < vector_len(list); i++) {
        void **pair = vector_get(list, i);
        Type *ty = pair[0];
        Node *expr = pair[1];
        if (type_compatible(contexpr->ty, ty))
            return expr;
    }
    if (!defaultexpr)
        errort(tok, "no matching generic selection for %s: %s", node2s(contexpr), ty2s(contexpr->ty));
    return defaultexpr;
}

/*
 * _Static_assert
 */

static void read_static_assert() {
    expect('(');
    int val = read_intexpr();
    expect(',');
    Token *tok = get();
    if (tok->kind != sym(string))
        errort(tok, "string expected as the second argument for _Static_assert, but got %s", tok2s(tok));
    expect(')');
    expect(';');
    if (!val)
        errort(tok, "_Static_assert failure: %s", tok->sval);
}

/*
 * Expression
 */

static Node *read_var_or_func(char *name) {
    Node *v = get(env(), name);
    if (!v) {
        Token *tok = peek();
        if (!is_keyword(tok, '('))
            errort(tok, "undefined variable: %s", name);
        Type *ty = make_func_type(type_int, allocate_vector(h), true, false);
        warnt(tok, "assume returning int: %s()", name);
        return ast_funcdesg(ty, name);
    }
    if (v->ty->kind == sym(func))
        return ast_funcdesg(v->ty, name);
    return v;
}

static int get_compound_assign_op(Token *tok) {
    if (tok->kind != sym(keyword))
        return 0;
    return(tok->id)
        }

static Node *read_stmt_expr() {
    Node *r = read_compound_stmt();
    expect(')');
    Type *rtype = type_void;
    if (vector_len(r->stmts) > 0) {
        Node *lastexpr = vector_tail(r->stmts);
        if (lastexpr->ty)
            rtype = lastexpr->ty;
    }
    r->ty = rtype;
    return r;
}

static Node *read_primary_expr() {
    Token *tok = get();
    if (!tok) return NULL;
    if (is_keyword(tok, '(')) {
        if (next_token('{'))
            return read_stmt_expr();
        Node *r = read_expr();
        expect(')');
        return r;
    }
    if (is_keyword(tok, KGENERIC)) {
        return read_generic();
    }
    switch (tok->kind) {
    case TIDENT:
        return read_var_or_func(tok->sval);
    case TNUMBER:
        return read_number(tok);
    case TCHAR:
        return ast_inttype(char_type(tok->enc), tok->c);
    case TSTRING:
        return ast_string(tok->enc, tok->sval, tok->slen);
    case TKEYWORD:
        unget_token(tok);
        return NULL;
    default:
        error("internal error: unknown token kind: %d", tok->kind);
    }
}

static Node *read_subscript_expr(Node *node) {
    Token *tok = peek();
    Node *sub = read_expr();
    if (!sub)
        errort(tok, "subscription expected");
    expect(']');
    Node *t = binop('+', conv(node), conv(sub));
    return ast_uop(sym(deref), t->ty->ptr, t);
}

static Node *read_postfix_expr_tail(Node *node) {
    if (!node) return NULL;
    for (;;) {
        if (next_token('(')) {
            Token *tok = peek();
            node = conv(node);
            Type *t = node->ty;
            if (t->kind != sym(ptr) || t->ptr->kind != sym(func))
                errort(tok, "function expected, but got %s", node2s(node));
            node = read_funcall(node);
            continue;
        }
        if (next_token('[')) {
            node = read_subscript_expr(node);
            continue;
        }
        if (next_token('.')) {
            node = read_struct_field(node);
            continue;
        }
        if (next_token(sym(->))) {
            if (node->ty->kind != "ptr")
                error("pointer type expected, but got %s %s",
                      ty2s(node->ty), node2s(node));
            node = ast_uop(sym(deref), node->ty->ptr, node);
            node = read_struct_field(node);
            continue;
        }
        Token *tok = peek();
        if (next_token(sym(inc)) || next_token(sym(dec))) {
            ensure_lvalue(node);
            int op = is_keyword(tok, OP_INC) ? OP_POST_INC : OP_POST_DEC;
            return ast_uop(op, node->ty, node);
        }
        return node;
    }
}

static Node *read_postfix_expr() {
    Node *node = read_primary_expr();
    return read_postfix_expr_tail(node);
}

static Node *read_unary_incdec(int op) {
    Node *operand = read_unary_expr();
    operand = conv(operand);
    ensure_lvalue(operand);
    return ast_uop(op, operand->ty, operand);
}

static Node *read_label_addr(Token *tok) {
    // [GNU] Labels as values. You can get the address of the a label
    // with unary "&&" operator followed by a label name.
    Token *tok2 = get();
    if (tok2->kind != TIDENT)
        errort(tok, "label name expected after &&, but got %s", tok2s(tok2));
    Node *r = ast_label_addr(tok2->sval);
    vector_push(gotos, r);
    return r;
}

static Node *read_unary_addr() {
    Node *operand = read_cast_expr();
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

static Node *read_unary_expr() {
    Token *tok = get();
    if (tok->kind == TKEYWORD) {
        switch (tok->id) {
        case KSIZEOF: return read_sizeof_operand();
        case KALIGNOF: return read_alignof_operand();
        case OP_INC: return read_unary_incdec(OP_PRE_INC);
        case OP_DEC: return read_unary_incdec(OP_PRE_DEC);
        case OP_LOGAND: return read_label_addr(tok);
        case '&': return read_unary_addr();
        case '*': return read_unary_deref(tok);
        case '+': return read_cast_expr();
        case '-': return read_unary_minus();
        case '~': return read_unary_bitnot(tok);
        case '!': return read_unary_lognot();
        }
    }
    unget_token(tok);
    return read_postfix_expr();
}

static Node *read_compound_literal(Type *ty) {
    char *name = make_label();
    vector init = read_decl_init(ty);
    Node *r = ast_lvar(ty, name);
    r->lvarinit = init;
    return r;
}

static Type *read_cast_type() {
    return read_abstract_declarator(read_decl_spec(NULL));
}

static Node *read_cast_expr() {
    Token *tok = get();
    if (is_keyword(tok, '(') && is_type(peek())) {
        Type *ty = read_cast_type();
        expect(')');
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

static Node *read_assignment_expr() {
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

static Node *read_comma_expr() {
    Node *node = read_assignment_expr();
    while (next_token(',')) {
        Node *expr = read_assignment_expr();
        node = ast_binop(expr->ty, ',', node, expr);
    }
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

/*
 * Struct or union
 */

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

static char *read_rectype_tag() {
    Token *tok = get();
    if (tok->kind == TIDENT)
        return tok->sval;
    unget_token(tok);
    return NULL;
}

static int compute_padding(int offset, int align) {
    return (offset % align == 0) ? 0 : align - offset % align;
}

static void squash_unnamed_struct(tuple dict, Type *unnamed, int offset) {
    vector keys = dict_keys(unnamed->fields);
    for (int i = 0; i < vector_len(keys); i++) {
        char *name = vector_get(keys, i);
        Type *t = copy_type(dict_get(unnamed->fields, name));
        t->offset += offset;
        dict_put(dict, name, t);
    }
}

static int read_bitsize(char *name, Type *ty) {
    if (!is_inttype(ty))
        error("non-integer type cannot be a bitfield: %s", ty2s(ty));
    Token *tok = peek();
    int r = read_intexpr();
    int maxsize = ty->kind == sym(boolean) ? 1 : ty->size * 8;
    if (r < 0 || maxsize < r)
        errort(tok, "invalid bitfield size for %s: %d", ty2s(ty), r);
    if (r == 0 && name != NULL)
        errort(tok, "zero-width bitfield needs to be unnamed: %s", name);
    return r;
}

static vector read_rectype_fields_sub() {
    vector r = allocate_vector(h);
    for (;;) {
        if (next_token(KSTATIC_ASSERT)) {
            read_static_assert();
            continue;
        }
        if (!is_type(peek()))
            break;
        Type *basetype = read_decl_spec(NULL);
        if (basetype->kind == sym(struct) && next_token(';')) {
            vector_push(r, make_pair(NULL, basetype));
            continue;
        }
        for (;;) {
            char *name = NULL;
            Type *fieldtype = read_declarator(&name, basetype, NULL, DECL_PARAM_TYPEONLY);
            ensure_not_void(fieldtype);
            fieldtype = copy_type(fieldtype);
            fieldtype->bitsize = next_token(':') ? read_bitsize(name, fieldtype) : -1;
            vector_push(r, make_pair(name, fieldtype));
            if (next_token(','))
                continue;
            if (is_keyword(peek(), '}'))
                warnt(peek(), "missing ';' at the end of field list");
            else
                expect(';');
            break;
        }
    }
    expect('}');
    return r;
}

static void fix_rectype_flexible_member(vector fields) {
    for (int i = 0; i < vector_len(fields); i++) {
        void **pair = vector_get(fields, i);
        char *name = pair[0];
        Type *ty = pair[1];
        if (ty->kind != sym(array))
            continue;
        if (ty->len == -1) {
            if (i != vector_len(fields) - 1)
                error("flexible member may only appear as the last member: %s %s", ty2s(ty), name);
            if (vector_len(fields) == 1)
                error("flexible member with no other fields: %s %s", ty2s(ty), name);
            ty->len = 0;
            ty->size = 0;
        }
    }
}

static void finish_bitfield(int *off, int *bitoff) {
    *off += (*bitoff + 7) / 8;
    *bitoff = 0;
}

static tuple update_struct_offset(int *rsize, int *align, vector fields) {
    int off = 0, bitoff = 0;
    tuple r = timm();
    for (int i = 0; i < vector_len(fields); i++) {
        void **pair = vector_get(fields, i);
        char *name = pair[0];
        Type *fieldtype = pair[1];
        // C11 6.7.2.1p14: Each member is aligned to its natural boundary.
        // As a result the entire struct is aligned to the largest among its members.
        // Unnamed fields will never be accessed, so they shouldn't be taken into account
        // when calculating alignment.
        if (name)
            *align = MAX(*align, fieldtype->align);

        if (name == NULL && fieldtype->kind == sym(struct)) {
            // C11 6.7.2.1p13: Anonymous struct
            finish_bitfield(&off, &bitoff);
            off += compute_padding(off, fieldtype->align);
            squash_unnamed_struct(r, fieldtype, off);
            off += fieldtype->size;
            continue;
        }
        if (fieldtype->bitsize == 0) {
            // C11 6.7.2.1p12: The zero-size bit-field indicates the end of the
            // current run of the bit-fields.
            finish_bitfield(&off, &bitoff);
            off += compute_padding(off, fieldtype->align);
            bitoff = 0;
            continue;
        }
        if (fieldtype->bitsize > 0) {
            int bit = fieldtype->size * 8;
            int room = bit - (off * 8 + bitoff) % bit;
            if (fieldtype->bitsize <= room) {
                fieldtype->offset = off;
                fieldtype->bitoff = bitoff;
            } else {
                finish_bitfield(&off, &bitoff);
                off += compute_padding(off, fieldtype->align);
                fieldtype->offset = off;
                fieldtype->bitoff = 0;
            }
            bitoff += fieldtype->bitsize;
        } else {
            finish_bitfield(&off, &bitoff);
            off += compute_padding(off, fieldtype->align);
            fieldtype->offset = off;
            off += fieldtype->size;
        }
        if (name)
            dict_put(r, name, fieldtype);
    }
    finish_bitfield(&off, &bitoff);
    *rsize = off + compute_padding(off, *align);
    return r;
}

static tuple update_union_offset(int *rsize, int *align, vector fields) {
    int maxsize = 0;
    tuple r = timm();
    for (int i = 0; i < vector_len(fields); i++) {
        void **pair = vector_get(fields, i);
        char *name = pair[0];
        Type *fieldtype = pair[1];
        maxsize = MAX(maxsize, fieldtype->size);
        *align = MAX(*align, fieldtype->align);
        if (name == NULL && fieldtype->kind == sym(struct)) {
            squash_unnamed_struct(r, fieldtype, 0);
            continue;
        }
        fieldtype->offset = 0;
        if (fieldtype->bitsize >= 0)
            fieldtype->bitoff = 0;
        if (name)
            dict_put(r, name, fieldtype);
    }
    *rsize = maxsize + compute_padding(maxsize, *align);
    return r;
}

static Dict *read_rectype_fields(int *rsize, int *align, boolean is_struct) {
    if (!next_token('{'))
        return NULL;
    vector fields = read_rectype_fields_sub();
    fix_rectype_flexible_member(fields);
    if (is_struct)
        return update_struct_offset(rsize, align, fields);
    return update_union_offset(rsize, align, fields);
}

static Type *read_rectype_def(boolean is_struct) {
    char *tag = read_rectype_tag();
    Type *r;
    if (tag) {
        r = get(tags, tag);
        if (r && (r->kind == sym(enum) || r->is_struct != is_struct))
            error("declarations of %s does not match", tag);
        if (!r) {
            r =  make_type(&(Type){ is_struct?sym(struct):sym(union)});
            set(tags, tag, r);
        }
    } else {
        r =  make_type(&(Type){ is_struct?sym(struct):sym(union)});            
    }
    int size = 0, align = 1;
    tuple fields = read_rectype_fields(&size, &align, is_struct);
    r->align = align;
    if (fields) {
        r->fields = fields;
        r->size = size;
    }
    return r;
}

static Type *read_struct_def() {
    return read_rectype_def(true);
}

static Type *read_union_def() {
    return read_rectype_def(false);
}

/*
 * Enum
 */

static Type *read_enum_def() {
    char *tag = NULL;
    Token *tok = get();

    // Enum is handled as a synonym for int. We only check if the enum
    // is declared.
    if (tok->kind == TIDENT) {
        tag = tok->sval;
        tok = get();
    }
    if (tag) {
        Type *ty = get(tags, tag);
        if (ty && ty->kind != sym(enum))
            errort(tok, "declarations of %s does not match", tag);
    }
    if (!is_keyword(tok, '{')) {
        if (!tag || !get(tags, tag))
            errort(tok, "enum tag %s is not defined", tag);
        unget_token(tok);
        return type_int;
    }
    if (tag)
        set(tags, tag, type_enum);

    int val = 0;
    for (;;) {
        tok = get();
        if (is_keyword(tok, '}'))
            break;
        if (tok->kind != TIDENT)
            errort(tok, "identifier expected, but got %s", tok2s(tok));
        char *name = tok->sval;

        if (next_token('='))
            val = read_intexpr();
        Node *constval = ast_inttype(type_int, val++);
        set(env(), name, constval);
        if (next_token(','))
            continue;
        if (next_token('}'))
            break;
        errort(peek(), "',' or '}' expected, but got %s", tok2s(peek()));
    }
    return type_int;
}

/*
 * Initializer
 */

static void assign_string(vector inits, Type *ty, char *p, int off) {
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
        char *fieldname;
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
                char *s = vector_get(keys, i++);
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
            int idx = read_intexpr();
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

/*
 * Declarator
 *
 * C's syntax for declaration is not only hard to read for humans but also
 * hard to parse for hand-written parsers. Consider the following two cases:
 *
 *   A: int *x;
 *   B: int *x();
 *
 * A is of type pointer to int, but B is not a pointer type B is of type
 * function returning a pointer to an integer. The meaning of the first half
 * of the declaration ("int *" part) is different between them.
 *
 * In 8cc, delcarations are parsed by two functions: read_declarator
 * and read_declarator_tail. The former function parses the first half of a
 * declaration, and the latter parses the (possibly nonexistent) parentheses
 * of a function or an array.
 */

static Type *read_func_param(char **name, boolean optional) {
    int sclass = 0;
    Type *basety = type_int;
    if (is_type(peek())) {
        basety = read_decl_spec(&sclass);
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
            expect(')');
            *ellipsis = true;
            return;
        }
        char *name;
        Type *ty = read_func_param(&name, typeonly);
        ensure_not_void(ty);
        vector_push(types, ty);
        if (!typeonly)
            vector_push(vars, ast_lvar(ty, name));
        tok = get();
        if (is_keyword(tok, ')'))
            return;
        if (!is_keyword(tok, ','))
            errort(tok, "comma expected, but got %s", tok2s(tok));
    }
}

static Type *read_func_param_list(vector paramvars, Type *rettype) {
    // C11 6.7.6.3p10: A parameter list with just "void" specifies that
    // the function has no parameters.
    Token *tok = get();
    if (is_keyword(tok, KVOID) && next_token(')'))
        return make_func_type(rettype, allocate_vector(h), false, false);

    // C11 6.7.6.3p14: K&R-style un-prototyped declaration or
    // function definition having no parameters.
    // We return a type representing K&R-style declaration here.
    // If this is actually part of a declartion, the type will be fixed later.
    if (is_keyword(tok, ')'))
        return make_func_type(rettype, allocate_vector(h), true, true);
    unget_token(tok);

    Token *tok2 = peek();
    if (next_token(KELLIPSIS))
        errort(tok2, "at least one parameter is required before \"...\"");
    if (is_type(peek())) {
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
        len = read_intexpr();
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
    if (next_token('('))
        return read_declarator_func(basety, params);
    return basety;
}

static void skip_type_qualifiers() {
    while (next_token(KCONST) || next_token(KVOLATILE) || next_token(KRESTRICT));
}

// C11 6.7.6: Declarators
static Type *read_declarator(char **rname, Type *basety, vector params, int ctx) {
    if (next_token('(')) {
        // '(' is either beginning of grouping parentheses or of a function parameter list.
        // If the next token is a type name, a parameter list must follow.
        if (is_type(peek()))
            return read_declarator_func(basety, params);
        // If not, it's grouping. In that case we have to read from outside.
        // For example, consider int (*)(), which is "pointer to function returning int".
        // We have only read "int" so far. We don't want to pass "int" to
        // a recursive call, or otherwise we would get "pointer to int".
        // Here, we pass a dummy object to get "pointer to <something>" first,
        // continue reading to get "function returning int", and then combine them.
        Type *stub = make_stub_type();
        Type *t = read_declarator(rname, stub, params, ctx);
        expect(')');
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

// C11 6.7.7: Type names
// read_abstract_declarator reads a type name.
// A type name is a declaration that omits the identifier.
// A few examples are int* (pointer to int), int() (function returning int),
// int*() (function returning pointer to int),
// or int(*)() (pointer to function returning int). Used for casting.
static Type *read_abstract_declarator(Type *basety) {
    return read_declarator(NULL, basety, NULL, DECL_CAST);
}

/*
 * typeof()
 */

static Type *read_typeof() {
    expect('(');
    Type *r = is_type(peek())
        ? read_cast_type()
        : read_comma_expr()->ty;
    expect(')');
    return r;
}

/*
 * Declaration specifier
 */

static boolean is_poweroftwo(int x) {
    // If there's only one bit set in x, the value is a power of 2.
    return (x <= 0) ? false : !(x & (x - 1));
}

static int read_alignas() {
    // C11 6.7.5. Valid form of _Alignof is either _Alignas(type-name) or
    // _Alignas(constant-expression).
    expect('(');
    int r = is_type(peek())
        ? read_cast_type()->align
        : read_intexpr();
    expect(')');
    return r;
}

static Type *read_decl_spec(int *rsclass) {
    int sclass = 0;
    Token *tok = peek();
    if (!is_type(tok))
        errort(tok, "type name expected, but got %s", tok2s(tok));

    Type *usertype = NULL;
    enum { kvoid = 1, kboolean, kchar, kint, kfloat, kdouble } kind = 0;
    enum { kshort = 1, klong, kllong } size = 0;
    enum { ksigned = 1, kunsigned } sig = 0;
    int align = -1;

    for (;;) {
        tok = get();
        if (tok->kind == EOF)
            error("premature end of input");
        if (kind == 0 && tok->kind == TIDENT && !usertype) {
            Type *def = get_typedef(tok->sval);
            if (def) {
                if (usertype) goto err;
                usertype = def;
                goto errcheck;
            }
        }
        if (tok->kind != TKEYWORD) {
            unget_token(tok);
            break;
        }
        switch (tok->id) {
        case KTYPEDEF:  if (sclass) goto err; sclass = S_TYPEDEF; break;
        case KEXTERN:   if (sclass) goto err; sclass = S_EXTERN; break;
        case KSTATIC:   if (sclass) goto err; sclass = S_STATIC; break;
        case KAUTO:     if (sclass) goto err; sclass = S_AUTO; break;
        case KREGISTER: if (sclass) goto err; sclass = S_REGISTER; break;
        case KCONST:    break;
        case KVOLATILE: break;
        case KINLINE:   break;
        case KNORETURN: break;
        case KVOID:     if (kind) goto err; kind = kvoid; break;
        case KBOOLEAN:     if (kind) goto err; kind = kboolean; break;
        case KCHAR:     if (kind) goto err; kind = kchar; break;
        case KINT:      if (kind) goto err; kind = kint; break;
        case KFLOAT:    if (kind) goto err; kind = kfloat; break;
        case KDOUBLE:   if (kind) goto err; kind = kdouble; break;
        case KSIGNED:   if (sig) goto err; sig = ksigned; break;
        case KUNSIGNED: if (sig) goto err; sig = kunsigned; break;
        case KSHORT:    if (size) goto err; size = kshort; break;
        case KSTRUCT:   if (usertype) goto err; usertype = read_struct_def(); break;
        case KUNION:    if (usertype) goto err; usertype = read_union_def(); break;
        case KENUM:     if (usertype) goto err; usertype = read_enum_def(); break;
        case KALIGNAS: {
            int val = read_alignas();
            if (val < 0)
                errort(tok, "negative alignment: %d", val);
            // C11 6.7.5p6: alignas(0) should have no effect.
            if (val == 0)
                break;
            if (align == -1 || val < align)
                align = val;
            break;
        }
        case KLONG: {
            if (size == 0) size = klong;
            else if (size == klong) size = kllong;
            else goto err;
            break;
        }
        case KTYPEOF: {
            if (usertype) goto err;
            usertype = read_typeof();
            break;
        }
        default:
            unget_token(tok);
            goto done;
        }
    errcheck:
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
    switch (kind) {
    case kvoid:   ty = type_void; goto end;
    case kboolean:   ty = make_numtype(KIND_BOOLEAN, false); goto end;
    case kchar:   ty = make_numtype(KIND_CHAR, sig == kunsigned); goto end;
    case kfloat:  ty = make_numtype(KIND_FLOAT, false); goto end;
    case kdouble: ty = make_numtype(size == klong ? KIND_LDOUBLE : KIND_DOUBLE, false); goto end;
    default: break;
    }
    switch (size) {
    case kshort: ty = make_numtype(KIND_SHORT, sig == kunsigned); goto end;
    case klong:  ty = make_numtype(KIND_LONG, sig == kunsigned); goto end;
    case kllong: ty = make_numtype(KIND_LLONG, sig == kunsigned); goto end;
    default:     ty = make_numtype(KIND_INT, sig == kunsigned); goto end;
    }
    error("internal error: kind: %d, size: %d", kind, size);
 end:
    if (align != -1)
        ty->align = align;
    return ty;
 err:
    errort(tok, "type mismatch: %s", tok2s(tok));
}

/*
 * Declaration
 */

static void read_static_local_var(Type *ty, char *name) {
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
    if (is_type(peek()))
        return read_decl_spec(sclass);
    warnt(peek(), "type specifier missing, assuming int");
    return type_int;
}

static void read_decl(vector block, boolean isglobal) {
    int sclass = 0;
    Type *basetype = read_decl_spec_opt(&sclass);
    if (next_token(';'))
        return;
    for (;;) {
        char *name = NULL;
        Type *ty = read_declarator(&name, copy_incomplete_type(basetype), NULL, DECL_BODY);
        ty->isstatic = (sclass == S_STATIC);
        if (sclass == S_TYPEDEF) {
            ast_typedef(ty, name);
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

/*
 * Function definition
 */

static Node *read_func_body(Type *functype, char *fname, vector params) {
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
        if (is_keyword(tok, ')'))
            return;
        if (is_keyword(tok, '('))
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
        if (is_type(tok))
            continue;
        if (is_keyword(tok, '(')) {
            skip_parentheses(buf);
            continue;
        }
        if (tok->kind != TIDENT)
            continue;
        if (!is_keyword(peek(), '('))
            continue;
        vector_push(buf, get());
        skip_parentheses(buf);
        r = (is_keyword(peek(), '{') || is_type(peek()));
        break;
    }
    while (vector_len(buf) > 0)
        unget_token(vector_pop(buf));
    return r;
}

static void backfill_labels() {
    for (int i = 0; i < vector_len(gotos); i++) {
        Node *src = vector_get(gotos, i);
        char *label = src->label;
        Node *dst = get(labels, label);
        if (!dst)
            error("stray %s: %s", src->kind == sym(goto) ? "goto" : "unary &&", label);
        if (dst->newlabel)
            src->newlabel = dst->newlabel;
        else
            src->newlabel = dst->newlabel = make_label();
    }
}

static Node *read_funcdef() {
    int sclass = 0;
    Type *basetype = read_decl_spec_opt(&sclass);
    localenv = make_map_parent(globalenv);
    gotos = allocate_vector(h);
    labels = make_map();
    char *name;
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

/*
 * If
 */

static Node *read_booleanean_expr() {
    Node *cond = read_expr();
    return cond;
}

static Node *read_if_stmt() {
    expect('(');
    Node *cond = read_booleanean_expr();
    expect(')');
    Node *then = read_stmt();
    if (!next_token(KELSE))
        return ast_if(cond, then, NULL);
    Node *els = read_stmt();
    return ast_if(cond, then, els);
}

/*
 * For
 */

static Node *read_opt_decl_or_stmt() {
    if (next_token(';'))
        return NULL;
    vector list = allocate_vector(h);
    read_decl_or_stmt(list);
    return ast_compound_stmt(list);
}

#define SET_JUMP_LABELS(cont, brk)              \
    char *ocontinue = lcontinue;                \
    char *obreak = lbreak;                      \
    lcontinue = cont;                           \
    lbreak = brk

#define RESTORE_JUMP_LABELS()                   \
    lcontinue = ocontinue;                      \
    lbreak = obreak

static Node *read_for_stmt() {
    expect('(');
    char *beg = make_label();
    char *mid = make_label();
    char *end = make_label();
    tuple *orig = localenv;
    localenv = make_map_parent(localenv);
    Node *init = read_opt_decl_or_stmt();
    Node *cond = read_expr_opt();
    expect(';');
    Node *step = read_expr_opt();
    expect(')');
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

/*
 * While
 */

static Node *read_while_stmt() {
    expect('(');
    Node *cond = read_booleanean_expr();
    expect(')');

    char *beg = make_label();
    char *end = make_label();
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

/*
 * Do
 */

static Node *read_do_stmt() {
    char *beg = make_label();
    char *end = make_label();
    SET_JUMP_LABELS(beg, end);
    Node *body = read_stmt();
    RESTORE_JUMP_LABELS();
    Token *tok = get();
    if (!is_keyword(tok, KWHILE))
        errort(tok, "'while' is expected, but got %s", tok2s(tok));
    expect('(');
    Node *cond = read_boolean_expr();
    expect(')');
    expect(';');

    vector v = allocate_vector(h);
    vector_push(v, ast_dest(beg));
    if (body)
        vector_push(v, body);
    vector_push(v, ast_if(cond, ast_jump(beg), NULL));
    vector_push(v, ast_dest(end));
    return ast_compound_stmt(v);
}

/*
 * Switch
 */

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

#define SET_SWITCH_CONTEXT(brk)                 \
    vector ocases = cases;                      \
    char *odefaultcase = defaultcase;           \
    char *obreak = lbreak;                      \
    cases = allocate_vector(h);                 \
    defaultcase = NULL;                         \
    lbreak = brk

#define RESTORE_SWITCH_CONTEXT()                \
    cases = ocases;                             \
    defaultcase = odefaultcase;                 \
    lbreak = obreak

static Node *read_switch_stmt() {
    expect('(');
    Node *expr = conv(read_expr());
    ensure_inttype(expr);
    expect(')');

    char *end = make_label();
    SET_SWITCH_CONTEXT(end);
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
    RESTORE_SWITCH_CONTEXT();
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

static Node *read_case_label(Token *tok) {
    if (!cases)
        errort(tok, "stray case label");
    char *label = make_label();
    int beg = read_intexpr();
    if (next_token(KELLIPSIS)) {
        int end = read_intexpr();
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

static Node *read_default_label(Token *tok) {
    expect(':');
    if (defaultcase)
        errort(tok, "duplicate default");
    defaultcase = make_label();
    return read_label_tail(ast_dest(defaultcase));
}

/*
 * Jump statements
 */

static Node *read_break_stmt(Token *tok) {
    expect(';');
    if (!lbreak)
        errort(tok, "stray break statement");
    return ast_jump(lbreak);
}

static Node *read_continue_stmt(Token *tok) {
    expect(';');
    if (!lcontinue)
        errort(tok, "stray continue statement");
    return ast_jump(lcontinue);
}

static Node *read_return_stmt() {
    Node *retval = read_expr_opt();
    expect(';');
    if (retval)
        return ast_return(ast_conv(current_func_type->rettype, retval));
    return ast_return(NULL);
}

static Node *read_goto_stmt() {
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

static Node *read_label(Token *tok) {
    char *label = tok->sval;
    if (get(labels, label))
        errort(tok, "duplicate label: %s", tok2s(tok));
    Node *r = ast_label(label);
    set(labels, label, r);
    return read_label_tail(r);
}

/*
 * Statement
 */

static Node *read_stmt() {
    Token *tok = get();
    if (tok->kind == TKEYWORD) {
        switch (tok->id) {
        case '{':       return read_compound_stmt();
        case KIF:       return read_if_stmt();
        case KFOR:      return read_for_stmt();
        case KWHILE:    return read_while_stmt();
        case KDO:       return read_do_stmt();
        case KRETURN:   return read_return_stmt();
        case KSWITCH:   return read_switch_stmt();
        case KCASE:     return read_case_label(tok);
        case KDEFAULT:  return read_default_label(tok);
        case KBREAK:    return read_break_stmt(tok);
        case KCONTINUE: return read_continue_stmt(tok);
        case KGOTO:     return read_goto_stmt();
        }
    }
    if (tok->kind == TIDENT && next_token(':'))
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
    if (is_type(tok)) {
        read_decl(list, false);
    } else if (next_token(KSTATIC_ASSERT)) {
        read_static_assert();
    } else {
        Node *stmt = read_stmt();
        if (stmt)
            vector_push(list, stmt);
    }
}

/*
 * Compilation unit
 */

vector read_toplevels() {
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

/*
 * Token handling
 */

// C11 5.1.1.2p6 Adjacent string literal tokens are concatenated.
static void concatenate_string(Token *tok) {
    int enc = tok->enc;
    buffer b = make_buffer();
    buf_append(b, tok->sval, tok->slen - 1);
    while (peek()->kind == sym(string)) {
        Token *tok2 = read_token();
        buf_append(b, tok2->sval, tok2->slen - 1);
        int enc2 = tok2->enc;
        if (enc != ENC_NONE && enc2 != ENC_NONE && enc != enc2)
            errort(tok2, "unsupported non-standard concatenation of string literals: %s", tok2s(tok2));
        if (enc == ENC_NONE)
            enc = enc2;
    }
    buf_write(b, '\0');
    tok->sval = buf_body(b);
    tok->slen = buf_len(b);
    tok->enc = enc;
}

static Token *token_get() {
    Token *r = read_token();
    if (r->kind == TINVALID)
        errort(r, "stray character in program: '%c'", r->c);
    if (r->kind == sym(string) && peek()->kind == sym(string))
        concatenate_string(r);
    return r;
}

static Token *token_peek() {
    return peek_token();
}

/*
 * Initializer
 */

static void define_builtin(char *name, Type *rettype, vector paramtypes) {
    ast_gvar(make_func_type(rettype, paramtypes, true, false), name);
}

void parse_init(heap h) {
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
}
