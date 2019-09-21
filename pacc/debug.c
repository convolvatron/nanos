// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include "8cc.h"

static string decorate_int(heap h, char *name, Type *ty) {
    char *u = (ty->usig) ? "u" : "";
    if (ty->bitsize > 0)
        return aprintf(h, "%s%s:%d:%d", u, name, ty->bitoff, ty->bitoff + ty->bitsize);
    return aprintf(h, "%s%s", u, name);
}


#define sstring staticbuffer
static string string_from_type(heap h, tuple dict, Type *ty) {
    if (!ty)
        return sstring("(nil)");
    switch (ty->kind) {
    case KIND_VOID: return sstring("void");
    case KIND_BOOLEAN: return sstring("_Bool");
    case KIND_CHAR: return decorate_int(h, "char", ty);
    case KIND_SHORT: return decorate_int(h, "short", ty);
    case KIND_INT:  return decorate_int(h, "int", ty);
    case KIND_LONG: return decorate_int(h, "long", ty);
    case KIND_LLONG: return decorate_int(h, "llong", ty);
    case KIND_FLOAT: return sstring("float");
    case KIND_DOUBLE: return sstring("double");
    case KIND_LDOUBLE: return sstring("long double");
    case KIND_PTR:
        return aprintf(h, "*%s", string_from_type(h, dict, ty->ptr));
    case KIND_ARRAY:
        return aprintf(h, "[%d]%s", ty->len, string_from_type(h, dict, ty->ptr));
    case KIND_STRUCT: {
        char *kind = ty->is_struct ? "struct" : "union";
        symbol key = intern(aprintf(transient, "%p", ty));
        if (get(dict, key))
            return aprintf(h, "(%s)", kind);
        set(dict, key, true);
        if (ty->fields) {
            buffer b = allocate_buffer(h, 10);
            bprintf(b, "(%s", kind);
            table_foreach(ty->fields, fkey, ftype) 
                bprintf(b, " (%s)", do_ty2s(dict, ftype));
            bprintf(b, ")");
            return b;
        }
    }
    case KIND_FUNC: {
        buffer b = allocate_buffer(h);
        bprintf(b, "(");
        if (ty->params) {
            for (int i = 0; i < vec_len(ty->params); i++) {
                if (i > 0)
                    bprintf(b, ",");
                Type *t = vec_get(ty->params, i);
                bprintf(b, "%s", do_ty2s(dict, t));
            }
        }
        bprintf(b, ")=>%s", do_ty2s(dict, ty->rettype));
        return buf_body(b);
    }
    default:
        return aprintf("(Unknown ty: %d)", ty->kind);
    }
}

char *ty2s(Type *ty) {
    return do_ty2s(make_dict(), ty);
}

static void uop_to_string(buffer b, char *op, Node *node) {
    bprintf(b, "(%s %s)", op, node2s(node->operand));
}

static void binop_to_string(buffer b, char *op, Node *node) {
    bprintf(b, "(%s %s %s)", op, node2s(node->left), node2s(node->right));
}

static void a2s_declinit(buffer b, vector initlist) {
    for (int i = 0; i < vec_len(initlist); i++) {
        if (i > 0)
            bprintf(b, " ");
        Node *init = vec_get(initlist, i);
        bprintf(b, "%s", node2s(init));
    }
}

static void do_node2s(buffer b, Node *node) {
    if (!node) {
        bprintf(b, "(nil)");
        return;
    }
    switch (node->kind) {
    case AST_LITERAL:
        switch (node->ty->kind) {
        case KIND_CHAR:
            if (node->ival == '\n')      bprintf(b, "'\n'");
            else if (node->ival == '\\') bprintf(b, "'\\\\'");
            else if (node->ival == '\0') bprintf(b, "'\\0'");
            else bprintf(b, "'%c'", node->ival);
            break;
        case KIND_INT:
            bprintf(b, "%d", node->ival);
            break;
        case KIND_LONG:
            bprintf(b, "%ldL", node->ival);
            break;
        case KIND_LLONG:
            bprintf(b, "%lldL", node->ival);
            break;
        case KIND_FLOAT:
        case KIND_DOUBLE:
        case KIND_LDOUBLE:
            bprintf(b, "%f", node->fval);
            break;
        case KIND_ARRAY:
            bprintf(b, "\"%s\"", quote_cstring(node->sval));
            break;
        default:
            error("internal error");
        }
        break;
    case AST_LABEL:
        bprintf(b, "%s:", node->label);
        break;
    case AST_LVAR:
        bprintf(b, "lv=%s", node->varname);
        if (node->lvarinit) {
            bprintf(b, "(");
            a2s_declinit(b, node->lvarinit);
            bprintf(b, ")");
        }
        break;
    case AST_GVAR:
        bprintf(b, "gv=%s", node->varname);
        break;
    case AST_FUNCALL:
    case AST_FUNCPTR_CALL: {
        bprintf(b, "(%s)%s(", ty2s(node->ty),
                   node->kind == AST_FUNCALL ? node->fname : node2s(node));
        for (int i = 0; i < vec_len(node->args); i++) {
            if (i > 0)
                bprintf(b, ",");
            bprintf(b, "%s", node2s(vec_get(node->args, i)));
        }
        bprintf(b, ")");
        break;
    }
    case AST_FUNCDESG: {
        bprintf(b, "(funcdesg %s)", node->fname);
        break;
    }
    case AST_FUNC: {
        bprintf(b, "(%s)%s(", ty2s(node->ty), node->fname);
        for (int i = 0; i < vec_len(node->params); i++) {
            if (i > 0)
                bprintf(b, ",");
            Node *param = vec_get(node->params, i);
            bprintf(b, "%s %s", ty2s(param->ty), node2s(param));
        }
        bprintf(b, ")");
        do_node2s(b, node->body);
        break;
    }
    case AST_GOTO:
        bprintf(b, "goto(%s)", node->label);
        break;
    case AST_DECL:
        bprintf(b, "(decl %s %s",
                   ty2s(node->declvar->ty),
                   node->declvar->varname);
        if (node->declinit) {
            bprintf(b, " ");
            a2s_declinit(b, node->declinit);
        }
        bprintf(b, ")");
        break;
    case AST_INIT:
        bprintf(b, "%s@%d", node2s(node->initval), node->initoff, ty2s(node->totype));
        break;
    case AST_CONV:
        bprintf(b, "(conv %s=>%s)", node2s(node->operand), ty2s(node->ty));
        break;
    case AST_IF:
        bprintf(b, "(if %s %s",
                   node2s(node->cond),
                   node2s(node->then));
        if (node->els)
            bprintf(b, " %s", node2s(node->els));
        bprintf(b, ")");
        break;
    case AST_TERNARY:
        bprintf(b, "(? %s %s %s)",
                   node2s(node->cond),
                   node2s(node->then),
                   node2s(node->els));
        break;
    case AST_RETURN:
        bprintf(b, "(return %s)", node2s(node->retval));
        break;
    case AST_COMPOUND_STMT: {
        bprintf(b, "{");
        for (int i = 0; i < vec_len(node->stmts); i++) {
            do_node2s(b, vec_get(node->stmts, i));
            bprintf(b, ";");
        }
        bprintf(b, "}");
        break;
    }
    case AST_STRUCT_REF:
        do_node2s(b, node->struc);
        bprintf(b, ".");
        bprintf(b, node->field);
        break;
    case AST_ADDR:  uop_to_string(b, "addr", node); break;
    case AST_DEREF: uop_to_string(b, "deref", node); break;
    case OP_SAL:  binop_to_string(b, "<<", node); break;
    case OP_SAR:
    case OP_SHR:  binop_to_string(b, ">>", node); break;
    case OP_GE:  binop_to_string(b, ">=", node); break;
    case OP_LE:  binop_to_string(b, "<=", node); break;
    case OP_NE:  binop_to_string(b, "!=", node); break;
    case OP_PRE_INC: uop_to_string(b, "pre++", node); break;
    case OP_PRE_DEC: uop_to_string(b, "pre--", node); break;
    case OP_POST_INC: uop_to_string(b, "post++", node); break;
    case OP_POST_DEC: uop_to_string(b, "post--", node); break;
    case OP_LOGAND: binop_to_string(b, "and", node); break;
    case OP_LOGOR:  binop_to_string(b, "or", node); break;
    case OP_A_ADD:  binop_to_string(b, "+=", node); break;
    case OP_A_SUB:  binop_to_string(b, "-=", node); break;
    case OP_A_MUL:  binop_to_string(b, "*=", node); break;
    case OP_A_DIV:  binop_to_string(b, "/=", node); break;
    case OP_A_MOD:  binop_to_string(b, "%=", node); break;
    case OP_A_AND:  binop_to_string(b, "&=", node); break;
    case OP_A_OR:   binop_to_string(b, "|=", node); break;
    case OP_A_XOR:  binop_to_string(b, "^=", node); break;
    case OP_A_SAL:  binop_to_string(b, "<<=", node); break;
    case OP_A_SAR:
    case OP_A_SHR:  binop_to_string(b, ">>=", node); break;
    case '!': uop_to_string(b, "!", node); break;
    case '&': binop_to_string(b, "&", node); break;
    case '|': binop_to_string(b, "|", node); break;
    case OP_CAST: {
        bprintf(b, "((%s)=>(%s) %s)",
                   ty2s(node->operand->ty),
                   ty2s(node->ty),
                   node2s(node->operand));
        break;
    }
    case OP_LABEL_ADDR:
        bprintf(b, "&&%s", node->label);
        break;
    default: {
        char *left = node2s(node->left);
        char *right = node2s(node->right);
        if (node->kind == OP_EQ)
            bprintf(b, "(== ");
        else
            bprintf(b, "(%c ", node->kind);
        bprintf(b, "%s %s)", left, right);
    }
    }
}

char *node2s(Node *node) {
    buffer b = make_buffer();
    do_node2s(b, node);
    return buf_body(b);
}

static char *encoding_prefix(int enc) {
    switch (enc) {
    case ENC_CHAR16: return "u";
    case ENC_CHAR32: return "U";
    case ENC_UTF8:   return "u8";
    case ENC_WCHAR:  return "L";
    }
    return "";
}

string string_from_token(Token *tok) {
    if (!tok)
        return "(null)";
    switch (tok->kind) {
    case TIDENT:
        return tok->sval;
    case TKEYWORD:
        switch (tok->id) {
#define op(id, str)         case id: return str;
#define keyword(id, str, _) case id: return str;
#include "keyword.inc"
#undef keyword
#undef op
        default: return aprintf("%c", tok->id);
        }
    case TCHAR:
        return aprintf("%s'%s'",
                      encoding_prefix(tok->enc),
                      quote_char(tok->c));
    case TNUMBER:
        return tok->sval;
    case TSTRING:
        return aprintf("%s\"%s\"",
                      encoding_prefix(tok->enc),
                      quote_cstring(tok->sval));
    case TEOF:
        return "(eof)";
    case TINVALID:
        return aprintf("%c", tok->c);
    case TNEWLINE:
        return "(newline)";
    case TSPACE:
        return "(space)";
    case TMACRO_PARAM:
        return "(macro-param)";
    }
    error("internal error: unknown token kind: %d", tok->kind);
}
