#include <runtime.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

int main(int argc, char **argv)
{
    heap h = init_process_runtime();
    table t = allocate_tuple();
    set(t, sym(foo), sym(bar));
    rprintf("%v\n", t);
}
