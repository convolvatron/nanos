typedef enum {
    HTTP_REQUEST_METHOD_GET = 0,
    HTTP_REQUEST_METHOD_HEAD,
    HTTP_REQUEST_METHOD_POST,
    HTTP_REQUEST_METHOD_PUT,
    HTTP_REQUEST_METHOD_DELETE,
    HTTP_REQUEST_METHOD_TRACE,
    HTTP_REQUEST_METHOD_OPTIONS,
    HTTP_REQUEST_METHOD_CONNECT,
    HTTP_REQUEST_METHOD_PATCH,
    HTTP_REQUEST_METHODS
} http_method;

typedef closure_type(http_response, void, tuple);
typedef closure_type(value_handler, void, value);

buffer_handler allocate_http_parser(heap h, value_handler each);
// just format the buffer?
status http_request(heap h, buffer_handler bh, tuple headers);
status send_http_response(buffer_handler out,
                          tuple t,
                          buffer c);
status send_http_chunk(buffer_handler out, buffer c);
status send_http_chunked_response(buffer_handler out, tuple t);
status send_http_response(buffer_handler out, tuple t, buffer c);

extern const char *http_request_methods[];

typedef struct http_listener *http_listener;
typedef closure_type(http_request_handler, void, http_method, buffer_handler, value);

void http_register_uri_handler(http_listener hl, const char *uri, http_request_handler each);
void http_register_default_handler(http_listener hl, http_request_handler each);
connection_handler connection_handler_from_http_listener(http_listener hl);
http_listener allocate_http_listener(heap h, u16 port);
void deallocate_http_listener(heap h, http_listener hl);

// ??
void init_management_http(heap h, tuple root);
    
string base64_encode(heap h, buffer x);

buffer_handler websocket_send_upgrade(heap h,
                                      table props,
                                      buffer_handler down,
                                      buffer_handler *in,
                                      buffer_handler up);

buffer_handler parse_json(heap h, value_handler j);

void format_json(buffer b, value v);


#define sstring(__x) ({ \
     static int init = 0;\
     /* must mark immutable? - is wrapped check*/\
     static struct buffer b;\
     if (!init) {\
      b.start = 0;\
      b.end = sizeof(__x) -1;\
      b.contents = __x;\
      b.length = b.end;\
      init = 1;\
     }        \
     &b;\
    })
