typedef table tuple;
typedef struct encoder *encoder;
typedef struct dencoder *dencoder;

void init_tuples(heap theap);
void print_tuple(buffer b, tuple t);
void print_root(buffer b, tuple t);

tuple allocate_tuple();
static inline void deallocate_tuple(tuple t)
{
    deallocate_table(t);
}

static inline void clear_tuple(tuple t)
{
    table_clear(t);
}

void destruct_tuple(tuple t, boolean recursive);

void encode_tuple(buffer dest, table dictionary, tuple t, u64 *total);


// h is for the bodies, the space for symbols and tuples are both implicit
void *decode_value(heap h, tuple dictionary, buffer source, u64 *total,
                   u64 *obsolete);
void encode_eav(buffer dest, table dictionary, tuple e, symbol a, value v,
                u64 *obsolete);

// seriously reconsider types allowed in tuples.. in particular simple
// ints have an anambiguous translation back and forth to strings (?)
static inline boolean u64_from_value(value v, u64 *result)
{
    return parse_int(alloca_wrap((buffer)v), 10, result);
}

static inline value value_from_u64(heap h, u64 v)
{
    value result = allocate_buffer(h, 10);
    print_number((buffer)result, v, 10, 0);
    return result;
}

static inline tuple find_or_allocate_tuple(tuple t, symbol s)
{
    assert(tagof(t) == tag_tuple);
    value v = table_find(t, s);
    if (!v)
        return allocate_tuple();
    assert(tagof(v) == tag_tuple);
    return (tuple)v;
}

typedef closure_type(tuple_generator, tuple);
typedef closure_type(tuple_set, void, value);
typedef closure_type(tuple_get, value, value);
typedef closure_type(binding_handler, void, value, value);
typedef closure_type(tuple_iterate, void, binding_handler);

typedef struct function_tuple {
    tuple_get g;
    tuple_set s;
    tuple_iterate i;                
} *function_tuple;

static inline value get(value v, symbol s)
{
    switch (tagof(v)){
    case tag_tuple:
        return table_find(v, s);
    case tag_function_tuple:
        {
            function_tuple ft = v;
            return apply(ft->g, s);
        }
        //vector!
    default:
        halt("get of non-gettable");
    }
}

