#include <runtime.h>

static heap mheap;

// we dont need to keep this hash table, from tree-land we are only
// ever reaching forward through the closures. the only time we need
// to get back to tree from pointerland is when we are cleaning up -
// already an issue with the hash map approach.

void set_management(void *obj,
                    tuple_set s,
                    tuple_get g,
                    tuple_iterate i)
{
    
}

void set_management_simple(void *obj, tuple_generator t)
{
    
}

function_tuple wrap_function(tuple_get g, tuple_set s, tuple_iterate i)
{
    function_tuple f = allocate_zero(mheap, sizeof(struct function_tuple));
    f->g = g;
    f->s = s;
    f->i = i;
    return f;
    
}

// this is the tagged tuple heap, so yeah, I guess we do want this
void init_management(heap h)
{
    mheap = h;
}

