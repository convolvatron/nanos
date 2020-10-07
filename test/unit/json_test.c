#include <runtime.h>
#include <http.h>

char *j1[] = {"{\"foo\":\"bar\"}",
              "{\"foo\":\"bar\", \"zed\":{\"kor\":{\"lop\":\"heg\", \"kez\":\"tir\"}}}"
};

// move to runtime
boolean value_equal(value a, value b)
{
    if (tagof(a) != tagof(b)) return false;
    switch (tagof(a)) {
    case tag_tuple:
        if (table_elements(a) != table_elements(b)) return false;
        table_foreach((table)a, k, v) 
            if (!value_equal(table_find((table)b, k), v)) return false;
        break;
    case tag_string:
        {
            u64 len = buffer_length((buffer)a);
            if ((len != buffer_length((buffer)b)) || 
                (runtime_memcmp(buffer_ref((buffer)a, 0), buffer_ref((buffer)b, 0), len)))
                return false;
        }
    }
    return true;
}

closure_function(1, 1, void, check2, value, t1, value, t2)
{
    if (!value_equal(bound(t1), t2)) {
        halt("mismatch %t %t\n", bound(t1), t2); 
    }
}

closure_function(1, 1, void, check1, heap, h, value, v)
{
    heap h = bound(h);
    buffer b = aprintf(h, "%t\n", v);
    buffer_write_byte(b, 0);
    console(buffer_ref(b, 0));
    buffer out = allocate_buffer(h, 1024);
    format_json(out, v);
    rprintf("fj: %b\n", out);
    apply(parse_json(h, closure(h, check2, v)), out);
}


int main()
{
    heap h = init_process_runtime();
    for (int i = 0 ;i < sizeof(j1)/sizeof(char *); i++) 
        apply(parse_json(h, closure(h, check1, h)), aprintf(h, "%s", j1[i])) ;
    return 0;
}
