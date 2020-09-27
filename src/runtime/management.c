#include <runtime.h>

static heap mheap;

void set_management(void *obj,
                    tuple_set s,
                    tuple_get g,
                    tuple_iterate i)
{
    
}

void set_management_simple(void *obj, tuple_generator t)
{
    
}

void init_management(heap h)
{
    mheap = h;
}
