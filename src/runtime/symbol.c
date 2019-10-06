#include <runtime.h>

static table symbols;
static heap sheap;
static heap iheap;

struct symbol {
    string s;
    key k;
};

symbol intern_u64(u64 u)
{
    buffer b = little_stack_buffer(10);
    print_number(b, u, 10, 0);
    return intern(b);
}

symbol intern(string name)
{
    symbol s;
    if (!(s = table_find(symbols, name))) {
        buffer b = allocate_string(iheap, buffer_length(name));
        if (b == INVALID_ADDRESS)
            goto alloc_fail;
        push_buffer(b, name);
        s = allocate(sheap, sizeof(struct symbol));
        if (s == INVALID_ADDRESS)
            goto alloc_fail;
        symbol n = valueof(s);
        n->k = random_u64();
        n->s = b;
        table_set(symbols, b, s);
    }
    return valueof(s);
  alloc_fail:
    halt("intern: alloc fail\n");
}

string symbol_string(symbol s)
{
    return s->s;
}

key key_from_symbol(void *z)
{
    symbol s = z;
    return s->k;
}



static value symget(value m, symbol b){return 0;}
static u64 symelements(value m){return 0;}
static void symset(value m, symbol b, value v){} // shouldn't set 
static void symformat(buffer b, value m){vformat(b, symbol_string(m));}
static void symiterate(heap h, value v, each e){iterate(h, symbol_string(v), e);}

static struct methods _symm = {symget, symset, symiterate, symformat, symelements};

void init_symbols(heap h, heap init)
{
    sheap = h;
    iheap = init;    
    symbols = allocate_table(iheap, fnv64, buffer_compare);
    tagmethods[tag_symbol] = &_symm;
}

