// Copyright 2012 Rui Ueyama. Released under the MIT license.

#include "nc.h"

heap transient; // xx -elim
#define sstring staticbuffer

// just keep this in the type
static string string_from_type(Type *ty) {
    if (!ty)
        return sstring("(nil)");

    if (ty->kind == sym(ptr))
        return aprintf(transient, "*%s", string_from_type(ty->ptr));
        
    if (ty->kind == sym(array))                 
        return aprintf(transient, "[%d]%s", ty->len, string_from_type(ty->ptr));
    
    if ((ty->kind == sym(union)) || (ty->kind == sym(struct)))  {
        //        symbol key = intern(aprintf(transient, "%p", ty));
        if (ty->fields) {
            buffer b = allocate_buffer(transient, 10);
            bprintf(b, "(%s", ty->kind);
            // ordering of struct entries .. matters semantically
            table_foreach(ty->fields, fkey, ftype) 
                bprintf(b, " (%s)", string_from_type(ftype));
            bprintf(b, ")");
            return b;
        }
    }
    
    if (ty->kind == sym(func)) {
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
    return symbol_string(ty->kind);
}

static void node2s(buffer b, Node *node);

static void uop_to_string(buffer b, symbol op, Node *node)
{
    bprintf(b, "(%s ", op);
    node2s(b, node->operand);
    bprintf(b, ")");
}

static void binop_to_string(buffer b, symbol s, Node *node)
{
    bprintf(b, "(%b ", symbol_string(s));
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

static void node2s(buffer b, Node *n) {
    if (!n) {
        bprintf(b, "(nil)");
        return;
    }
    if (get(n->p->binops, n->kind)) {
        binop_to_string(b, n->kind, n);
    } else {
        if (get(n->p->unops, n->kind)) {
            uop_to_string(b, n->kind, n);                
        } else {
            if (n->kind == sym(literal)) {
                symbol ntk = n->ty->kind;
                if (ntk == sym(char)) {                
                    if (n->ival == '\n')      bprintf(b, "'\n'");
                    else if (n->ival == '\\') bprintf(b, "'\\\\'");
                    else if (n->ival == '\0') bprintf(b, "'\\0'");
                    else bprintf(b, "'%c'", n->ival);
                    return;
                }
                if ((ntk == sym(int)) ||
                    (ntk == sym(long)) ||
                    (ntk == sym(llong))) {
                    bprintf(b, "%d", n->ival);
                    return;
                }
                if (ntk == sym(array)) {
                    bprintf(b, "\"%b\"", n->sval);
                }
            }
            if (n->kind == sym(label)) {
                bprintf(b, "%s:", n->label);
                return;
            }
            if (n->kind ==  sym(lvar)){
                bprintf(b, "lv=%s", n->varname);
                if (n->lvarinit) {
                    bprintf(b, "(");
                    a2s_declinit(b, n->lvarinit);
                    bprintf(b, ")");
                }
            }
            if (n->kind == sym(gvar)) {
                bprintf(b, "gv=%s", n->varname);
                return;
            }

            // xx why are these really so different, isn't this just
            // an expression?
            if ((n->kind == sym(funcall)) || (n->kind == sym(funcptr_call))) {
                bprintf(b, "(%s)",string_from_type(n->ty));
                if (n->kind == sym(funcall))
                    buffer_append(b, n->fname->contents, buffer_length(n->fname));
                else
                    node2s(b, n);
                njoin(b, n->args, ',');
                bprintf(b, ")");
                return;
            }
            
            if (n->kind == sym(funcdesg)) {
                bprintf(b, "(funcdesg %s)", n->fname);
                return;
            }
            
            if (n->kind == sym(func)) {
                bprintf(b, "(%s)%s(", string_from_type(n->ty), n->fname);
                njoin(b, n->ty->params, ',');
                bprintf(b, ")");
                node2s(b, n->body);
                return;
            }
            if (n->kind ==  sym(goto)) {
                bprintf(b, "goto(%s)", n->label);
                return;
            }
            
            if (n->kind ==  sym(decl)){
                bprintf(b, "(decl %b %b",
                        string_from_type(n->declvar->ty),
                        n->declvar->varname);
                if (n->declinit) {
                    bprintf(b, " ");
                    a2s_declinit(b, n->declinit);
                }
                bprintf(b, ")");
                return;
            }
            
            if (n->kind ==  sym(init)) {
                node2s(b, n->initval);
                bprintf(b, "@%d%b", 
                        n->initoff,
                        string_from_type(n->totype));
                return;
            }

            if (n->kind ==  sym(conv)) {
                bprintf(b, "(conv ");
                node2s(b, n->operand);
                bprintf(b, " =>%s)", string_from_type(n->ty));
                return;
            }
            
            if (n->kind ==  sym(if)) {
                bprintf(b, "(if ");
                node2s(b, n->cond);
                bprintf(b, " ");            
                node2s(b, n->then);
                if (n->els) {
                    bprintf(b, " ");                 
                    node2s(b, n->els);
                }
                bprintf(b, ")");
                return;
            }
            
            if (n->kind ==  sym(ternary)) {
                bprintf(b, "(? ");
                node2s(b, n->cond),        
                    bprintf(b, " ");
                node2s(b, n->then),        
                    bprintf(b, " ");
                node2s(b, n->els);
                return;
            }

            if (n->kind ==  sym(return)) {
                bprintf(b, "(return ");
                node2s(b, n->retval);
                bprintf(b, ")");
                return;
            }
            
            if (n->kind ==  sym(compound_stmt)) {
                bprintf(b, "{");
                njoin(b, n->stmts, ';');                
                bprintf(b, "}");
                return;
            }
            
            if (n->kind ==  sym(struct_ref)){
                node2s(b, n->struc);
                bprintf(b, ".");
                push_buffer(b, n->field);
                return;
            }
            
            if (n->kind ==  sym(cast)) {
                bprintf(b, "((%b)=>(%b) ",
                        string_from_type(n->operand->ty),
                        string_from_type(n->ty));
                node2s(b, n->operand);
                bprintf(b, ")");
                return;
            }
            
            if (n->kind ==  sym(label_addr)) {
                bprintf(b, "&&%s", n->label);
                return;
            }
            
            if (n->kind == sym(=))
                bprintf(b, "(== ");
            else
                bprintf(b, "(%c ", n->kind);
            node2s(b, n->left);
            bprintf(b, " ");
            node2s(b, n->right);
            bprintf(b, ")");        
        }
    }
}

string string_from_token(heap h, Token *tok) {
    if (!tok) return staticbuffer("(null)");
    if (tok->kind == sym(indent)) return tok->sval;
    if (tok->kind == sym(keyword)) return symbol_string(tok->id);
    if (tok->kind == sym(char))  aprintf(h, "'%s'",  tok->c);
    return tok->sval;
}
