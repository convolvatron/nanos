#include <runtime.h>
#include <http/http.h>

#define outline(__b, __format, ...)\
    bbprintf(__b, staticbuffer(__format), ## __VA_ARGS__);\
    buffer_append(__b, "\r\n", 2);

typedef struct websocket {
    heap h;
    buffer reassembly;
    buffer_handler down;
    buffer_handler up;
    timer keepalive;
    u32 output_mask;
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
void websocket_send(websocket w, int opcode, buffer b, thunk t)
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
    apply(w->down, out, ignore); // reclaim - there was a callback here 
    apply(w->down, b, t);
}


static CLOSURE_2_0(send_keepalive, void, websocket, buffer);
static void send_keepalive(websocket w, buffer b)
{
    websocket_send(w, 0x9, b, ignore);
}

/*CLOSURE_1_2(websocket_output_frame, void, websocket, buffer, thunk);
void websocket_output_frame(websocket w, buffer b, thunk t)
{
    websocket_send(w, 1, b, t);
}
*/
static CLOSURE_1_2(websocket_input_frame, void, websocket, buffer, thunk);
static void websocket_input_frame(websocket w, buffer b, thunk t)
{
    int rlen;

    if (!b) {
        //        apply(w->client, 0, ignore);
        return;
    }

    // there is a better approach here, chained buffers, incremental delivery, etc
    buffer_append(w->reassembly, buffer_ref(b, 0), buffer_length(b));
    while ((rlen = buffer_length(w->reassembly)) > 0) {
        int offset = 2;
        if (rlen < offset) goto end;
        long length = *(u8 *)buffer_ref(w->reassembly, 1) & 0x7f;

        if (length == 126) {
            if (rlen < 4) goto end;
            length = __bswap16(*(u16 *)buffer_ref(w->reassembly, 2));
            offset += 2;
        } else {
            if (length == 127) {
                if (rlen< 10) goto end;
                length = __bswap64(*(u64 *)buffer_ref(w->reassembly, 2));
                offset += 8;
            }
        }

        int opcode = *(u8 *)buffer_ref(w->reassembly, 0) & 0xf;
        if (opcode == ws_close) {
            apply(w->up, 0, ignore);
            return;
        }
        u32 mask = 0;
        // which should always be the case for client streams
        if (*(u8 *)buffer_ref(w->reassembly, 1) & 0x80) {
            mask = *(u32 *)buffer_ref(w->reassembly, offset);
            offset += 4;
        }

        if ((rlen - offset) < length) goto end;

        w->reassembly->start += offset;

        if (mask) {
            for (int i=0; i<length; i++) {
                // xxx -figure out how to apply this a word at a time
                *(u8 *)buffer_ref(w->reassembly, i) ^= (mask>>((i&3)*8)) & 0xff;
            }
        }

        switch(opcode) {
        case ws_continuation:
            // prf("error - currently dont handle websocket continuation\n");
            break;
        case ws_text:
        case ws_binary:
            {
                buffer out = w->reassembly;
                // we'd like to use this buffer if we can, but
                // client is going to mess up our pointers
                // if (buffer_length(w->reassembly) > length)
                // leak?
                out = wrap_buffer(w->h, buffer_ref(w->reassembly, 0), length);
                apply(w->up, out, ignore);
                break;
            }
        case ws_ping:
            websocket_send(w, ws_pong, w->reassembly, ignore);
            break;
        case ws_close:
        case ws_pong:
            break;
            //        default:
            // prf("invalid ws frame %d\n", opcode);
        }
        w->reassembly->start += length;
        if (w->reassembly->start == w->reassembly->end) {
            buffer_clear(w->reassembly);
        }
    }
 end:
    // i think we're responsible for freeing this buffer
    apply(t);
}

void sha1(buffer d, buffer s);

websocket new_websocket(heap h)
{
    websocket w = allocate(h, sizeof(struct websocket));
    w->reassembly = allocate_buffer(h, 1000);
    w->h = h;
    w->output_mask = 0;
    //    w->self = cont(h, websocket_input_frame, w);
    return w;
}


/*
static CLOSURE_1_1(client_connected, void, websocket, buffer_handler) 
static void client_connected(websocket w, buffer_handler down)
{
    //    apply(w->down, response_header_parser(w->h, cont(w->h, header_response, w)));
}
*/

buffer_handler websocket_client(heap h, buffer_handler rx)
{
    websocket w = new_websocket(h);
    //    estring host = lookupv((edb)request, rid, sym(host));
    //    tcp_create_client (h,
    //                       station_from_string(h, alloca_wrap_buffer(host->body, host->length)),
    //                       cont(h, client_connected, w, request, rid));
    return(w->up);
}


buffer_handler websocket_send_upgrade(heap h,
                                      tuple r,
                                      buffer_handler down)
{
    websocket w = new_websocket(h);
    tuple headers = get(r, sym(headers));
    string ekey = get(headers, sym(Sec-WebSocket-Key));
    string proto = get(headers, sym(Sec-WebSocket-Protocol));
    string key;

    if (!ekey) return 0;
    key = allocate_buffer(h, ekey->length);
    buffer_append(key, ekey->contents, ekey->length);
    buffer k = staticbuffer("258EAFA5-E914-47DA-95CA-C5AB0DC85B11");
    buffer_append(key, k->contents, k->length);
    buffer sh = allocate_buffer(h, 20);
    sha1(sh, key);
    string encoded_key = base64_encode(h, sh);
    buffer upgrade = allocate_buffer(h, 200);

    outline(upgrade, "HTTP/1.1 101 Switching Protocols");
    outline(upgrade, "Upgrade: websocket");
    outline(upgrade, "Connection: Upgrade");
    outline(upgrade, "Sec-WebSocket-Accept: %b", encoded_key);
    if (proto)
        outline(upgrade, "Sec-WebSocket-Protocol: %r", proto);
    outline(upgrade, "");

    register_periodic_timer(seconds(5),
                            closure(w->h, send_keepalive, w, allocate_buffer(w->h, 0)));
    
    apply(down, upgrade, ignore);
    return closure(h, websocket_input_frame, w);
}
