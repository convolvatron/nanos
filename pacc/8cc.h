#include <runtime.h>


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
    int offset; // what is an offset?
    // bitfield
    int bitoff;
    int bitsize;
    // function
    struct Type *rettype;
    vector params; // params = fields
    boolean hasva;
} Type;


typedef struct parse {
    heap h;
    buffer b;
    tuple globalenv;
    tuple localenv;
    tuple tags;
    tuple labels;
    tuple types;
    
    vector toplevels;
    vector localvars;
    vector gotos;
    vector cases;
    Type *current_func_type;
    
    char *defaultcase;
    char *lbreak;
    char *lcontinue;
    
    Type *type_void, *type_bool, *type_char, *type_short, *type_int;
    Type  *type_long, *type_llong, *type_uchar, *type_ushort, *type_uint;
    Type *type_ulong, *type_ullong, *type_enum;

    tuple binops, unops; // spacify
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

typedef struct Node {
    symbol kind;
    Type *ty;
    location sourceLoc;
    parse p;
    union {
        // Char, int, or long
        long ival;
        // Float or double
        struct {
            double fval;
            char *flabel;
        };
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
            int initoff;
            Type *totype;
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


typedef struct {
    int beg;
    int end;
    buffer label;
} Case;

#define error(...)       
#define errort(tok, ...) 
#define warn(...)        
#define warnt(tok, ...)  

void errorf(char *line, char *pos, char *fmt, ...);
void warnf(char *line, char *pos, char *fmt, ...);

// lex.c
void lex_init(char *filename);
boolean is_keyword(Token *tok, symbol c);
Token *get_token(buffer b);
Token *lex_string(char *s);
Token *lex(void);

// parse.c
boolean is_inttype(Type *ty);
void *make_pair(heap h, void *first, void *second);
int eval_intexpr(Node *node, Node **addr);
Node *read_expr(void);
vector *read_toplevels(void);
void parse_init(heap);

