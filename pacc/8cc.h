#include <runtime.h>

typedef struct {
    symbol kind;
    buffer file;
    int line;
    int column;
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

typedef struct Type {
    symbol kind;
    int size;
    int align;
    boolean usig; // true if unsigned
    buffer name;    
    boolean isstatic;
    // pointer or array
    struct Type *ptr;
    // array length
    int len;
    vector fields;
    int offset;
    //     boolean is_struct; // true if struct, false if union
    // bitfield
    int bitoff;
    int bitsize;
    // function
    struct Type *rettype;
    vector params;
    boolean hasva;
    boolean oldstyle;
} Type;

typedef struct {
    char *file;
    int line;
} SourceLoc;

typedef struct Node {
    symbol kind;
    Type *ty;
    SourceLoc *sourceLoc;
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
            char *varname;
            // local
            int loff;
            vector lvarinit;
            // global
            char *glabel;
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
            char *label;
            char *newlabel;
        };
        // Return statement
        struct Node *retval;
        // Compound statement
        vector stmts;
        // Struct reference
        struct {
            struct Node *struc;
            char *field;
            Type *fieldtype;
        };
    };
} Node;

#define error(...)       
#define errort(tok, ...) 
#define warn(...)        
#define warnt(tok, ...)  

void errorf(char *line, char *pos, char *fmt, ...);
void warnf(char *line, char *pos, char *fmt, ...);

// lex.c
void lex_init(char *filename);
char *get_base_file(void);
void skip_cond_incl(void);
char *read_header_file_name(boolean *std);
boolean is_keyword(Token *tok, symbol c);
void token_buffer_stash(vector *buf);
void token_buffer_unstash();
void unget_token(Token *tok);
Token *lex_string(char *s);
Token *lex(void);

// parse.c
boolean is_inttype(Type *ty);
void *make_pair(void *first, void *second);
int eval_intexpr(Node *node, Node **addr);
Node *read_expr(void);
vector *read_toplevels(void);
void parse_init(void);

