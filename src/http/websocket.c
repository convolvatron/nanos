#include <runtime.h>
#include <http/http.h>

extern timerheap runloop_timers; // xxx - are there no userspace timers?

void outline(buffer b, char *s)
{
    buffer_write_cstring(b, s);
    buffer_write_byte(b, '\r');
    buffer_write_byte(b, '\n');
}

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
    apply(w->write, out); 
    apply(w->write, b);
}


closure_function(2, 1, void, send_keepalive,
                 websocket, w,
                 buffer, b,
                 u64, overruns)
{
    // refcnt?
    buffer b = clone_buffer(bound(w)->h, bound(b));
    websocket_send(bound(w), 0x9, b);
}

closure_function(1, 1, status, websocket_output_frame,
                 websocket, w,
                 buffer, b)
{
    websocket_send(bound(w), 1, b);
    return STATUS_OK;
}

closure_function(1, 1, status, websocket_input_frame,
                 websocket, w,
                 buffer, b)
{
    websocket w = bound(w);
    int offset = 2;

    if (!b) {
        apply(w->client, 0);
        return STATUS_OK;
    }

    // there is a better approach here, chained buffers, or at least assuming it will fit
    buffer_append(w->reassembly, buffer_ref(b, 0), buffer_length(b));
    int rlen = buffer_length(w->reassembly);
    if (rlen < offset) return STATUS_OK;

    u64 length = *(u8 *)buffer_ref(w->reassembly, 1) & 0x7f;

    if (length == 126) {
        if (rlen < 4) return STATUS_OK;
        buffer s = sub_buffer(transient, b, 2, 2);
        length = buffer_read_be16(s);
        offset += 2;
    } else {
        if (length == 127) {
            // ok, we are throwing away the top byte, who the hell thought
            // that 1TB wasn't enough per object
            if (rlen< 10) return STATUS_OK;
            buffer s = sub_buffer(transient, b, 2, 8);
            length = buffer_read_be64(s);            
            offset += 8;
        }
    }
    
    int opcode = *(u8 *)buffer_ref(w->reassembly, 0) & 0xf;
    
    u32 mask = 0;
    // which should always be the case for client streams
    if (*(u8 *)buffer_ref(w->reassembly, 1) & 0x80) {
        mask = *(u32 *)buffer_ref(b, offset);
        offset += 4;
    }

    if ((rlen - offset) >= length) {
        if (mask) {
            for (int i=0;i<((length +3)/4); i++) {
                // xxx - fallin off the end 
                *(u32 *)buffer_ref(w->reassembly, offset + i * 4) ^= mask;
            }
        }
        // xxx - only deliver this message
        // compress reassembly buffer

        w->reassembly->start += offset;
        switch(opcode) {
        case ws_text:
        case ws_binary:
            apply(w->client, w->reassembly);
            break;
        case ws_ping:
            websocket_send(w, ws_pong, w->reassembly);
            break;
        case ws_close:
        case ws_pong:
            break;
        default:
            rprintf("invalid ws frame %d\n", opcode);
        }
        w->reassembly->start += length;

        if((w->reassembly->start = w->reassembly->end)) {
            buffer_clear(w->reassembly);
        }
    }
    return STATUS_OK;
}

void sha1(buffer d, buffer s);

buffer_handler websocket_send_upgrade(heap h,
                                      table props,
                                      buffer_handler down,
                                      buffer_handler *in,
                                      buffer_handler up)
{
    websocket w = allocate(h, sizeof(struct websocket));
    string ekey;
    string key;
    if (!(ekey=get(props, sym(Sec-WebSocket-Key)))) {
        // something tasier on the error handling...shutdown
        // connection and have a bad edge
        return 0;
    } 

    // fix
    w->reassembly = allocate_buffer(h, 1000);
    w->write = down;
    w->client = up;
    w->h = h;

    char fixed_uuid[] = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
    key = allocate_buffer(h, ekey->length + sizeof(fixed_uuid));
    buffer_concat(key, ekey);
    // xxx - didn't extend?
    buffer_append(key, fixed_uuid, sizeof(fixed_uuid)-1);

    buffer sh = allocate_buffer(h, 20);
    sha1(sh, key);
    string r = base64_encode(h, sh);
    buffer upgrade = allocate_buffer(h, 200);

    // could use tuple
    outline(upgrade, "HTTP/1.1 101 Switching Protocols");
    outline(upgrade, "Upgrade: websocket");
    outline(upgrade, "Connection: Upgrade");
    buffer_write_cstring(upgrade, "Sec-WebSocket-Accept: ");
    buffer_concat(upgrade, r);
    outline(upgrade, "");    
    outline(upgrade, "");
    
    register_timer(runloop_timers, CLOCK_ID_MONOTONIC, 0, true, seconds(5),
                   closure(w->h, send_keepalive, w, allocate_buffer(w->h, 0)));
    apply(w->write, upgrade);
    w->self = closure(h, websocket_input_frame, w);
    *in = w->self;
    return(closure(h, websocket_output_frame, w));
}

