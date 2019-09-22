// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include "8cc.h"

//static string decorate_int(char *name, Type *ty) {
//    char *u = (ty->usig) ? "u" : "";
//    if (ty->bitsize > 0)
//        return aprintf(h, "%s%s:%d:%d", u, name, ty->bitoff, ty->bitoff + ty->bitsize);
//    return aprintf(h, "%s%s", u, name);
//}


heap transient; // xx -elim
#define sstring staticbuffer
// just keep this in the type
static string string_from_type(Type *ty) {
    if (!ty)
        return sstring("(nil)");
    switch (ty->kind) {
    case KIND_VOID: return sstring("void");
        
    case KIND_BOOLEAN: return sstring("boolean");
        // these look suspiciously the same
    case KIND_CHAR: return sstring("char");
    case KIND_SHORT: return sstring("short");
    case KIND_INT:  return sstring("int");
    case KIND_LONG: return sstring("long");
    case KIND_LLONG: return sstring("llong");
        
    case KIND_PTR:
        return aprintf(transient, "*%s", string_from_type(ty->ptr));
        
    case KIND_ARRAY:
        return aprintf(transient, "[%d]%s", ty->len, string_from_type(ty->ptr));
    // when to refer?
    case KIND_STRUCT: {
        char *kind = ty->is_struct ? "struct" : "union";
        //        symbol key = intern(aprintf(transient, "%p", ty));
        if (ty->fields) {
            buffer b = allocate_buffer(transient, 10);
            bprintf(b, "(%s", kind);
            table_foreach(ty->fields, fkey, ftype) 
                bprintf(b, " (%s)", string_from_type(ftype));
            bprintf(b, ")");
            return b;
        }
    }
    case KIND_FUNC: {
        buffer b = allocate_buffer(transient, 10);
        bprintf(b, "(");
        if (ty->params) {
            boolean first = true;
            Type *t;             
            vector_foreach(ty->params, t) {
                if (!first)  bprintf(b, ",");
                first = false;
                bprintf(b, "%s", string_from_type(t));
            }
        }
        bprintf(b, ")=>%s", string_from_type(ty->rettype));
        return b;
    }
    default:
        return aprintf(transient, "(Unknown ty: %d)", ty->kind);
    }
}

// shouldn't really need heap..
static void node2s(buffer b, Node *node);

static void uop_to_string(buffer b, char *op, Node *node) {
    bprintf(b, "(%s ", op);
    node2s(b, node->operand);
    bprintf(b, ")");
}

static void binop_to_string(buffer b, char *op, Node *node) {
    bprintf(b, "(%s ");
    node2s(b, node->left);
    bprintf(b, " ");
    node2s(b, node->right);
    bprintf(b, ")");    
}

static void a2s_declinit(buffer b, vector initlist) {
    Node *i;
    boolean first = true;
    vector_foreach(initlist, i) {
        if (!first) bprintf(b, " ");
        first = false;
        node2s(b, i);
    }
}

static void node2s(buffer b, Node *node) {
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
        case KIND_ARRAY:
            bprintf(b, "\"%b\"", node->sval);
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
        bprintf(b, "(%s)",string_from_type(node->ty));
        if (node->kind == AST_FUNCALL)
            buffer_append(b, node->fname->contents, buffer_length(node->fname));
        else
            node2s(b, node);
        // make a join        
        boolean first = true;
        value i;
        vector_foreach(node->args, i) {
            if (!first) bprintf(b, ",");
            first = false;
            node2s(b, i);
        }
        bprintf(b, ")");
        break;
    }
    case AST_FUNCDESG: {
        bprintf(b, "(funcdesg %s)", node->fname);
        break;
    }
    case AST_FUNC: {
        bprintf(b, "(%s)%s(", string_from_type(node->ty), node->fname);
        boolean first = true;
        Node *n;             
        vector_foreach(node->ty->params, n) {
            if (!first) bprintf(b, ",");
            first = false;
            bprintf(b, "%b ", string_from_type(n->ty));
            node2s(b, n);
        }
        bprintf(b, ")");
        node2s(b, node->body);
        break;
    }
    case AST_GOTO:
        bprintf(b, "goto(%s)", node->label);
        break;
    case AST_DECL:
        bprintf(b, "(decl %b %b",
                string_from_type(node->declvar->ty),
                node->declvar->varname);
        if (node->declinit) {
            bprintf(b, " ");
            a2s_declinit(b, node->declinit);
        }
        bprintf(b, ")");
        break;
    case AST_INIT:
        node2s(b, node->initval);
        bprintf(b, "@%d%b", 
                node->initoff,
                string_from_type(node->totype));
        break;
    case AST_CONV:
        // fix
        bprintf(b, "(conv %b=>%s)",
                node2s(b, node->operand),
                string_from_type(node->ty));
        break;
    case AST_IF:
        bprintf(b, "(if ");
        node2s(b, node->cond);
        bprintf(b, " ");            
        node2s(b, node->then);
        if (node->els) {
            bprintf(b, " ");                 
            node2s(b, node->els);
        }
        bprintf(b, ")");
        break;
    case AST_TERNARY:
        bprintf(b, "(? ");
        node2s(b, node->cond),        
            bprintf(b, " ");
        node2s(b, node->then),        
            bprintf(b, " ");
        node2s(b, node->els);        
        break;
    case AST_RETURN:
        bprintf(b, "(return %s)", node2s(node->retval));
        break;
    case AST_COMPOUND_STMT: {
        bprintf(b, "{");
        Node *i;
        vector_foreach(node->stmts, i) {
            node2s(b, i);
            bprintf(b, ";");
        }
        bprintf(b, "}");
        break;
    }
    case AST_STRUCT_REF:
        node2s(b, node->struc);
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
        bprintf(b, "((%b)=>(%b) ",
                string_from_type(node->operand->ty),
                string_from_type(node->ty));
        node2s(b, node->operand);
        bprintf(b, ")");
        break;
    }
    case OP_LABEL_ADDR:
        bprintf(b, "&&%s", node->label);
        break;
    default: {
        if (node->kind == OP_EQ)
            bprintf(b, "(== ");
        else
            bprintf(b, "(%c ", node->kind);
        node2s(b, node->left);
        bprintf(b, " ");
        node2s(b, node->right);
        bprintf(b, ")");        
    }
    }
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

string string_from_token(heap h, Token *tok) {
    if (!tok)
        return staticbuffer("(null)");
    switch (tok->kind) {
    case TIDENT:
        return tok->sval;
    case TKEYWORD:
        return symbol_string(tok->id);
    case TCHAR:
        return aprintf(h, "%s'%s'",
                       encoding_prefix(tok->enc),
                       quote_char(tok->c));
    case TNUMBER:
        return tok->sval;
    case TSTRING:
        return aprintf("%s\"%b\"",
                       encoding_prefix(tok->enc),
                       tok->sval);
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
