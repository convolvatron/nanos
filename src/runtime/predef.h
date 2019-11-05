#pragma once
typedef struct buffer *buffer;
typedef struct heap *heap;

// does clang have these?
#define varg  __builtin_va_arg
#define vlist  __builtin_va_list
#define vstart __builtin_va_start
#define vend __builtin_va_end


#define foreach_arg(__start, _x)                                       \
    for (__builtin_va_list _va; __builtin_va_start(_va, __start), 1; ) \
        for (void *_x;  _x=__builtin_va_arg(_va, value), _x != INVALID_ADDRESS; )
