#include <runtime.h>
#include <http/http.h>

extern thunk ignore;

typedef struct websocket {
    heap h;
    heap buffer_heap;
    buffer reassembly;
    buffer_handler client;
    buffer_handler write;
    timer keepalive;
    buffer_handler self;
} *websocket;

typedef enum {
    ws_continuation = 0,
    ws_text = 1,
    ws_binary = 2,
    ws_close = 8,
    ws_ping = 9,
    ws_pong = 10,
} opcodes;


// implement close
void websocket_send(websocket w, int opcode, buffer b)
{
    int length = buffer_length(b);
    buffer out = allocate_buffer(w->h, 10);
    buffer_write_byte(out, opcode | 0x80);
    if (length > 65536) {
        buffer_write_byte(out, 127);
        buffer_write_be64(out, length);
    } else {
        if (length > 125) {
            buffer_write_byte(out, 126);
            buffer_write_be16(out, length);
        } else {
            buffer_write_byte(out, length);
        }
    }
    apply(w->write, out); // reclaim
    apply(w->write, b);
}


closure_function(2, 0, void, send_keepalive,
                 websocket, w,
                 buffer, b)
{
    websocket_send(bound(w), 0x9, bound(b));
}

void websocket_output_frame(websocket w, buffer b)
{
    websocket_send(w, 1, b);
}

static void websocket_input_frame(websocket w, buffer b)
{
    int offset = 2;

    if (!b) {
        apply(w->client, 0);
        return;
    }

    // there is a better approach here, chained buffers, or at least assuming it will fit
    buffer_append(w->reassembly, buffer_ref(b, 0), buffer_length(b));
    int rlen = buffer_length(w->reassembly);
    if (rlen < offset) return;

    u64 length = *(u8 *)buffer_ref(w->reassembly, 1) & 0x7f;

    if (length == 126) {
        if (rlen < 4) return;
        length = htons(*(u16 *)bref(w->reassembly, 2));
        offset += 2;
    } else {
        if (length == 127) {
            // ok, we are throwing away the top byte, who the hell thought
            // that 1TB wasn't enough per object
            if (rlen< 10) return;
            length = htonll(*(u64 *)buffer_ref(w->reassembly, 2));
            offset += 8;
        }
    }
    
    int opcode = *(u8 *)bref(w->reassembly, 0) & 0xf;
    
    u32 mask = 0;
    // which should always be the case for client streams
    if (*(u8 *)bref(w->reassembly, 1) & 0x80) {
        mask = *(u32 *)bref(b, offset);
        offset += 4;
    }

    if ((rlen - offset) >= length) {
        if (mask) {
            for (int i=0;i<((length +3)/4); i++) {
                // xxx - fallin off the end 
                *(u32 *)bref(w->reassembly, offset + i * 4) ^= mask;
            }
        }
        // xxx - only deliver this message
        // compress reassembly buffer

        w->reassembly->start += offset;
        switch(opcode) {
        case ws_text:
        case ws_binary:
            apply(w->client, w->reassembly, ignore);
            break;
        case ws_ping:
            websocket_send(w, ws_pong, w->reassembly, ignore);
            break;
        case ws_close:
        case ws_pong:
            break;
        default:
            prf("invalid ws frame %d\n", opcode);
        }
        w->reassembly->start += length;

        if((w->reassembly->start = w->reassembly->end)) {
            buffer_clear(w->reassembly);
        }
    }
    apply(reg, w->self);
}

void sha1(buffer d, buffer s);

buffer_handler websocket_send_upgrade(heap h,
                                      buffer_handler down,
                                      buffer_handler up,
                                      register_read reg)
{
    websocket w = allocate(h, sizeof(struct websocket));
    estring ekey;
    string key;

    if (!(ekey=lookupv(b, n, sym(Sec-WebSocket-Key)))) {
        // something tasier
        return 0;
    } 

    key = allocate_buffer(h, ekey->length);
    buffer_append(key, ekey->body, ekey->length);
    
    // fix
    w->reassembly = allocate_buffer(h, 1000);
    w->write = down;
    w->client = up;
    w->h = h;

    string_concat(key, sstring("258EAFA5-E914-47DA-95CA-C5AB0DC85B11"));
    buffer sh = allocate_buffer(h, 20);
    sha1(sh, key);
    string r = base64_encode(h, sh);
    buffer upgrade = allocate_buffer(h, 200);

    outline(upgrade, "HTTP/1.1 101 Switching Protocols");
    outline(upgrade, "Upgrade: websocket");
    outline(upgrade, "Connection: Upgrade");
    outline(upgrade, "Sec-WebSocket-Accept: %b", r);
    outline(upgrade, "");

    register_periodic_timer(seconds(5), cont(w->h, send_keepalive, w, allocate_buffer(w->h, 0)));
    apply(w->write, upgrade, ignore);
    w->self = cont(h, websocket_input_frame, w);
    apply(reg, w->self);
    return(cont(h, websocket_output_frame, w));
}

