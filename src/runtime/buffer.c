#include <runtime.h>

buffer allocate_buffer(heap h, heap ch, void *contents, bytes s)
{
    buffer b = allocate(h, sizeof(struct buffer));
    if (b == INVALID_ADDRESS)
        return b;
    b->start = 0;
    b->end = 0;
    b->length = s;
    b->h = ch;
    b->contents = allocate(ch, s);
    if (b->contents == INVALID_ADDRESS) {
        deallocate(h, b, sizeof(struct buffer));
        return INVALID_ADDRESS;
    }
    return b;
}

void buffer_append(buffer b,
                     const void *body,
                     bytes length)
{
    buffer_extend(b, length);
    buffer_write(b, body, length);
}
