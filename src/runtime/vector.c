#include <runtime.h>

// these two should be asynchronous? dont you think?
static value sget(value m, symbol b)
{
    return 0;
}

static u64 selements(value m)
{
    return 0;
}

static void sset(value m, symbol b, value v)
{
}


static void sformat(buffer b, value m)
{
    push_buffer(b, m);
}

// static CLOSURE_2_0(seach, void, buffer, each);
static void seach(buffer b, each n)
{
    rprintf("actually calling seah\n");
    // self close
}

static void siterate(heap h, value v, each e)
{
    buffer in = (buffer)v;
    buffer b = wrap_buffer(h, in->contents, buffer_length(in));
    // allocate working buffer to record offset, and reclaim..on h i guess
    rprintf("actually calling titerate\n");
    seach(b, e);
}

static struct methods _vm = {vget, vset, viterate, vformat, velements};

void init_vector(heap h)
{
    string_heap = h;
    tagmethods[tag_string] = &_vm;
}
