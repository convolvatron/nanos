#include <runtime.h>

typedef struct scope *scope;

struct scope {
    tuple here;
    scope parent;
};
    
value scope_get(scope s, symbol k)
{
    value v;
    if ((v = get(s->here, k))) return v;
    return get(s->parent, k);
}

void scope_set(scope s, symbol k, value v)
{
    set(s->here, k, v);
}

// dispatch get/set/iterate - parent can be any map
scope allocate_scope(heap h, scope parent)
{
    scope s = allocate(h, sizeof(struct scope));
    return s;
}

value sget_internal(tuple t, ...)
{
    value v = t;
    // empty is a valid map
    foreach_arg(t, x) v = get(v, x);
    return v;
}
