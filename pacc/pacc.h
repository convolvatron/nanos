#include <runtime.h>

typedef struct scope *scope;

typedef tuple Type;
#if 0
typedef struct Type {
    symbol kind;
    int size;
    int align;
    boolean usig; // true if unsigned
    buffer name;    
    boolean isstatic;
    // pointer or array - soft instantiate!
    struct Type *ptr;
    // array length
    int len;
    vector fields;
    int bitsize; // is arraylen really?
    // function
    struct Type *rettype;
    vector params; // params = fields
    boolean hasva;
} Type;
#endif

typedef struct parse {
    heap h;
    buffer b;

    // umm
    scope env;
    scope file;
    scope global;    
} *parse;


typedef struct location {
    buffer file;
    int line;
    int column;
    int start, end;
} *location;

typedef struct {
    symbol kind;
    location s;
    union {
        // TKEYWORD - the same as kind really?
        symbol id;
        // TSTRING or TCHAR
        struct {
            buffer sval;
            int c;
        };
    };
} Token;

typedef tuple Node;

#if 0
typedef struct Node {
    symbol kind;
    Type *ty; // -> type
    location sourceLoc;
    //    parse p;
    union {
        // Char, int, or long
        long ival;
        // String
        struct {
            buffer sval;
            char *slabel;
        };
        // Local/global variable
        struct {
            buffer varname;
            // local
            int loff;
            vector lvarinit;
            // global
            buffer glabel;
        };
        // Binary operator
        struct {
            struct Node *left;
            struct Node *right;
        };
        // Unary operator
        struct {
            struct Node *operand;
        };
        // Function call or function declaration
        struct {
            string fname;
            // Function call
            vector args;
            struct Type *ftype;
            // Function pointer or function designator
            struct Node *fptr;
            // Function declaration
            vector params;
            vector localvars;
            struct Node *body;
        };
        // Declaration
        struct {
            struct Node *declvar;
            vector declinit;
        };
        // Initializer
        struct {
            struct Node *initval;
        };
        // If statement or ternary operator
        struct {
            struct Node *cond;
            struct Node *then;
            struct Node *els;
        };
        // Goto and label
        struct {
            buffer label;
            buffer newlabel;
        };
        // Return statement
        struct Node *retval;
        // Compound statement
        vector stmts;
        // Struct reference
        struct {
            struct Node *struc;
            buffer field;
            Type *fieldtype;
        };
    };
} Node;
#endif

typedef struct {
    int beg;
    int end;
    buffer label;
} Case;

tuple parse_init(heap h, buffer b);

#define error(...)       
#define errort(tok, ...) 
#define warn(...)        
#define warnt(tok, ...)  

void errorf(char *line, char *pos, char *fmt, ...);
void warnf(char *line, char *pos, char *fmt, ...);

// lex.c
boolean is_keyword(Token *tok, symbol c);
Token *get_token(buffer b);

// parse.c
boolean is_inttype(Type ty);
void *make_pair(heap h, void *first, void *second);
tuple parse_init(heap, buffer);

// really sget_internal
value sget_internal(value p, ...);
#define sget(first, ...)  sget_internal(first, __VA_ARGS__, INVALID_ADDRESS)
