

typedef closure_type(tuple_generator, tuple);
typedef closure_type(tuple_set, void, value);
typedef closure_type(tuple_get, value, value);
typedef closure_type(tuple_iterate, value, value);

typedef struct function_tuple {
    tuple_set s;
    tuple_get g;
    tuple_iterate i;                
} *function_tuple;
    


void set_management(void *obj,
                    tuple_set s,
                    tuple_get g,
                    tuple_iterate i);

void set_management_simple(void *obj, tuple_generator t);

    
void init_management(heap h);

