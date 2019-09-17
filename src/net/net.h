#define NET 1
#define NET_SYSCALLS 1

boolean netsyscall_init(unix_heaps uh);
// i suppose this should take a requesting address
typedef closure_type(connection_handler, buffer_handler, buffer_handler);
void listen_port(heap h, u16 port, connection_handler c);
