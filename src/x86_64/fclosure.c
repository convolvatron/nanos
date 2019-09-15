#include <runtime.h>
#include <synth.h>

// we dont have compile time reflection
typedef void(*function)();
#define numberof(__x) (sizeof(__x)/sizeof(*__x))

u64 abi[] = {7, 6, 2, 1, 8, 9};

#define foreach_vararg(__x, __n)\
    for (int __first = 1; __first;)\
        for (vlist __va; __first && ({vstart(__va, __n);1;}) ; __first = 0)\
            for(value __x; (__x = varg(__va, value)) != INVALID_ADDRESS;)    


function specialize_internal(heap h, function f, u64 total, ...)
{
    string result = false;
    u64 shift = 0;
    u64 stack_count = 0;
    //    boolean spill = false;
    
    foreach_vararg(i, total)  shift++;
    
    if (total  > numberof(abi)) {
        stack_count = total - numberof(abi);
        // 16 byte alignmnet .. we're going to add a return address, so if we're aligned now
        // we wont be afterwards
        if (!(stack_count & 1)) {
            stack_count++;
            // should be rsp-8
            push_register(result, 11);
        }
    }
    
    for (int i = total - 1; i >= shift; i--)  {
        if (i >= numberof(abi)) {
            push_register(result, abi[i-shift]);
        }  else {
            move_register(result, abi[i], abi[i-shift]);
        }
    }
    
    u64 count = 0;
    foreach_vararg(i, total)
        move_64_imm(result, abi[count++], (u64)i);        
    
    move_64_imm(result, 11, (u64)f);
    if (stack_count) {
        call_register(result, 11);
        add_register_u8_immediate(result, REGISTER_SP, stack_count*sizeof(u64));
        returnop(result);
    }
    return (function)result->contents;
}

