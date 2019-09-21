#include <runtime.h>
#include <lwip.h>
#include <tfs.h> // seems sad
#include <unix.h>
#include <net.h>

typedef struct direct{
    connection_handler new;
    struct tcp_pcb *p;
    heap h;
} *direct;
    
static CLOSURE_1_2(direct_send, void, struct tcp_pcb *, buffer, thunk);
static void direct_send( struct tcp_pcb *pcb, buffer b, thunk t)
{
    //    u64 len = tcp_sndbuf(g->pcb);
    // flags can force a stack copy or toggle push
    // pool?
    // uh oh.. copy?
    tcp_write(pcb, buffer_ref(b, 0), buffer_length(b), TCP_WRITE_FLAG_COPY);
}

err_t direct_input(void *z, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
    buffer_handler bh = z;
    // i guess this is a close?
    if (p) {
        apply(bh, alloca_wrap_buffer(p->payload, p->len));
        // not necessarily
        tcp_recved(pcb, p->len);
    }
    return ERR_OK;
}

static err_t direct_accept(void *z, struct tcp_pcb *pcb, err_t b)
{
    direct g = z;
    buffer_handler bh = apply(g->new, closure(g->h, direct_send, pcb));
    tcp_arg(pcb, bh);    
    tcp_recv(pcb, direct_input);
    return ERR_OK;
}

void listen_port(heap h, u16 port, connection_handler c)
{
    direct g = (direct) allocate(h, sizeof(struct direct));
    g->p = tcp_new_ip_type(IPADDR_TYPE_ANY);
    g->h = h;
    g->new = c;
    tcp_bind(g->p, IP_ANY_TYPE, port);
    g->p = tcp_listen(g->p);
    tcp_arg(g->p, g);    
    tcp_accept(g->p, direct_accept);    
}
