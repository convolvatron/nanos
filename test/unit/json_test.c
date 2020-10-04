#include <runtime.h>
#include <http.h>


char *j1 = "{\"foo\":\"bar\", \"zed\":{\"kor\":{\"lop\":\"heg\", \"kez\":\"tir\"}}}";

// move to runtime
boolean value_equal(value a, value b)
{
    if (tagof(a) != tagof(b)) return false;
    switch (tagof(a)) {
    case tag_tuple:
        if (table_elements(a) != table_elements(b)) return false;
        table_foreach((table)a, k, v) 
            if (table_find((table)b, k) != v) return false;
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
    apply(parse_json(h, closure(h, check2, v)), format_json(h, v));
}


int main()
{
    heap h = init_process_runtime();
    apply(parse_json(h, closure(h, check1, h)), aprintf(h, "%s", j1)) ;
    return 0;
}
