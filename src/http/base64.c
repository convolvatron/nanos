#include <runtime/runtime.h>

// fix globally
#define htonl(__x) __builtin_bswap32(__x)

static char *map="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

string base64_encode(heap h, buffer x)
{
    int length = buffer_length(x);
    string out = allocate_buffer(h, (length*3+10)/4);
    int bcount = 0;

    while(length > 0) {
        u32 triple = 0;
        // use read_be32?
        runtime_memcpy(&triple, buffer_ref(x, bcount), (length<3?length:3));
        triple = htonl(triple);
        triple >>= 8;
        bcount += 3;
        
        buffer_write_byte(out, map[(triple >> 18) & 0x3f]);
        buffer_write_byte(out, map[(triple >> 12) & 0x3f]);
        
        if (length == 1) 
            buffer_write_byte(out, '=');
        else 
            buffer_write_byte(out, map[(triple >> 6) & 0x3f]);
        
        if (length < 3) 
            buffer_write_byte(out, '=');
        else 
            buffer_write_byte(out, map[triple & 0x3f]);
        length -= 3;
    }
    return(out);
}

