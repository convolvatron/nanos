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

void wrap_function(heap h, tuple_get g) {
    
}

// this is the tagged tuple heap, so yeah, I guess we do want this
void init_management(heap h)
{
    mheap = h;
}

