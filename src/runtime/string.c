#include <runtime.h>

heap string_heap;

char *
runtime_strchr (const char *string, int _c)
{
    char c = _c;

    for (;;) 
        if (*string == c)
            return (char *) string;
        else if (*string == '\0')
            return 0;
        else
            string ++;
}
    
char *
runtime_strtok_r (char *s, const char *delimiters, char **save_ptr)
{
    char *token;

    if (s == 0)
        s = *save_ptr;

    while (runtime_strchr(delimiters, *s) != 0) {
        if (*s == '\0') {
            *save_ptr = s;
            return 0;
        }

        s ++;
    }

    token = s;
    while (runtime_strchr(delimiters, *s) == 0)
        s ++;

    if (*s != '\0') {
        *s = '\0';
        *save_ptr = s + 1;
    } else 
        *save_ptr = s;

    return token;
}

int
runtime_strcmp (const char *string1, const char *string2)
{
    while (*string1 && *string1 == *string2) {
        string1++;
        string2++;
    }

    return *(const unsigned char *)string1 - *(const unsigned char *)string2;
}


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

static struct methods _sm = {sget, sset, siterate, sformat, selements};

string allocate_string(heap h, int length)
{
    return allocate_buffer(string_heap, h, allocate(h, length), length);
}
    
void init_string(heap h)
{
    string_heap = h;
    tagmethods[tag_string] = &_sm;
}
