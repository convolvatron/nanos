#include <runtime.h>

struct scope {
    tuple here;
    scope parent;
};
    
value scope_get(scope s, symbol k)
{
    value v;
    if (v = get(s->here, k)) return v;
    return get(s->parent, k);
}

void scope_set(scope s, symbol k, value v)
{
    sert(s->here, k, v);
}

// dispatch get/set/iterate - parent can be any map
scope allocate_scope(heap h, scope parent)
{
    scope s = allocate(h, sizeof(struct scope));
    return s;
}

