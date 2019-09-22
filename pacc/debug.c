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
    case sym(void): return sstring("void");
        
    case sym(boolean): return sstring("boolean");
        // these look suspiciously the same
    case sym(char): return sstring("char");
    case sym(short): return sstring("short");
    case sym(int):  return sstring("int");
    case sym(long): return sstring("long");
    case sym(llong): return sstring("llong");
        
    case sym(ptr):
        return aprintf(transient, "*%s", string_from_type(ty->ptr));
        
    case sym(array):
        return aprintf(transient, "[%d]%s", ty->len, string_from_type(ty->ptr));
    // when to refer?
    case sym(union): {        
    case sym(struct): {
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
        case sym(func): {
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

static void node2s(buffer b, Node *node);

static void uop_to_string(buffer b, symbol op, Node *node) {
    bprintf(b, "(%s ", op);
    node2s(b, node->operand);
    bprintf(b, ")");
}

static void binop_to_string(buffer b, symbol s, Node *node) {
    bprintf(b, "(%b ", symbol_buffer(s));
    node2s(b, node->left);
    bprintf(b, " ");
    node2s(b, node->right);
    bprintf(b, ")");    
}


static void njoin(buffer dest, vector list, char sep)
{
    boolean first = true;
    value i;
    vector_foreach(list, i) {
        if (!first) buffer_write_byte(dest, sep);
        first = false;
        node2s(dest, i);
    }
}

static void a2s_declinit(buffer b, vector initlist) {
    njoin(b, initlist, ' ');
}

static void node2s(buffer b, Node *node) {
    if (!node) {
        bprintf(b, "(nil)");
        return;
    }
    if (get(binops, node->kind)) {
        binop_to_string(b, node->kind, node);
    } else {
        if (get(unops, node->kind)) {
            uop_to_string(b, node->kind, node);                
        } else {
            switch (node->kind) {
            case sym(literal):
                switch (node->ty->kind) {
                case sym(char):
                    if (node->ival == '\n')      bprintf(b, "'\n'");
                    else if (node->ival == '\\') bprintf(b, "'\\\\'");
                    else if (node->ival == '\0') bprintf(b, "'\\0'");
                    else bprintf(b, "'%c'", node->ival);
                    break;
                case sym(int):
                    bprintf(b, "%d", node->ival);
                    break;
                case sym(long):
                    bprintf(b, "%ldL", node->ival);
                    break;
                case sym(llong):
                    bprintf(b, "%lldL", node->ival);
                    break;
                case sym(array):
                    bprintf(b, "\"%b\"", node->sval);
                    break;
                default:
                    error("internal error");
                }
                break;
            case sym(label):
                bprintf(b, "%s:", node->label);
                break;
            case sym(lvar):
                bprintf(b, "lv=%s", node->varname);
                if (node->lvarinit) {
                    bprintf(b, "(");
                    a2s_declinit(b, node->lvarinit);
                    bprintf(b, ")");
                }
                break;
            case sym(gvar):
                bprintf(b, "gv=%s", node->varname);
                break;
                // xx why are these really so different, isn't this just
                // an expression?
            case sym(funcall):
            case sym(funcptr_call): {
                bprintf(b, "(%s)",string_from_type(node->ty));
                if (node->kind == AST_FUNCALL)
                    buffer_append(b, node->fname->contents, buffer_length(node->fname));
                else
                    node2s(b, node);
                njoin(b, node->args, ',');
                bprintf(b, ")");
                break;
            }
            case sym(funcdesg): {
                bprintf(b, "(funcdesg %s)", node->fname);
                break;
            }
            case sym(func): {
                bprintf(b, "(%s)%s(", string_from_type(node->ty), node->fname);
                njoin(b, node->ty->params, ',');
                bprintf(b, ")");
                node2s(b, node->body);
                break;
            }
            case sym(goto):
                bprintf(b, "goto(%s)", node->label);
                break;
            case sym(decl):
                bprintf(b, "(decl %b %b",
                        string_from_type(node->declvar->ty),
                        node->declvar->varname);
                if (node->declinit) {
                    bprintf(b, " ");
                    a2s_declinit(b, node->declinit);
                }
                bprintf(b, ")");
                break;
            case sym(init):
                node2s(b, node->initval);
                bprintf(b, "@%d%b", 
                        node->initoff,
                        string_from_type(node->totype));
                break;
            case sym(conv):
                // fix
                bprintf(b, "(conv ");
                node2s(b, node->operand);
                bprintf(b, " =>%s)", string_from_type(node->ty));
                break;
            case sym(if):
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
            case sym(ternary):
                bprintf(b, "(? ");
                node2s(b, node->cond),        
                    bprintf(b, " ");
                node2s(b, node->then),        
                    bprintf(b, " ");
                node2s(b, node->els);        
                break;
            case sym(return):
                bprintf(b, "(return ");
                node2s(b, node->retval);
                bprintf(b, ")");
                break;
            case sym(compound_stmt): {
                bprintf(b, "{");
                njoin(b, node->stmts, ';');                
                bprintf(b, "}");
                break;
            }
            case sym(struct_ref):
                node2s(b, node->struc);
                bprintf(b, ".");
                bprintf(b, node->field);
                break;
            case sym(cast): {
                bprintf(b, "((%b)=>(%b) ",
                        string_from_type(node->operand->ty),
                        string_from_type(node->ty));
                node2s(b, node->operand);
                bprintf(b, ")");
                break;
            }
            case sym(label_addr):
                bprintf(b, "&&%s", node->label);
                break;
            default: {
                if (node->kind == sym(=))
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
    }
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
        return tok->sval
        return b;
    case TEOF:
        return "(eof)";
    case TINVALID:
        return aprintf("%c", tok->c);
    case TNEWLINE:
        return "(newline)";
    }
    error("internal error: unknown token kind: %d", tok->kind);
}
