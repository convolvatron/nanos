#include <runtime.h>

void initialize_buffer();

static inline CLOSURE_0_0(ignore_body, void);
static inline void ignore_body(){}
thunk ignore;
status_handler ignore_status;

// maybe the same?
heap errheap;
heap transient;

void init_string(heap);

// init linker sets would clean up the platform dependency, if you link
// with it, it gets initialized
void init_runtime(kernel_heaps kh)
{
    
    // environment specific
    
    heap h = transient = heap_general(kh);
    init_tuples(allocate_tagged_region(kh, tag_tuple));
    init_string(allocate_tagged_region(kh, tag_string));
    init_vector(allocate_tagged_region(kh, tag_vector));    
    init_symbols(allocate_tagged_region(kh, tag_symbol), h);
    kh->method_rewind = allocate_tagged_region(kh, tag_method_rewind);
    ignore = closure(h, ignore_body);
    ignore_status = (void*)ignore;
    errheap = h;

    initialize_timers(kh);
}

#define STACK_CHK_GUARD 0x595e9fbd94fda766

u64 __stack_chk_guard = STACK_CHK_GUARD;

void __stack_chk_guard_init()
{
    __stack_chk_guard = random_u64();
}

void __attribute__((noreturn)) __stack_chk_fail(void)
{
    halt("stack check failed\n");
}
