#include <runtime.h>


enum {
    TIDENT,
    TKEYWORD,
    TNUMBER,
    TCHAR,
    TSTRING,
    TEOF,
    TINVALID,
    // Only in CPP
    MIN_CPP_TOKEN,
    TNEWLINE,
    TSPACE,
    TMACRO_PARAM,
};

enum {
    ENC_NONE,
    ENC_CHAR16,
    ENC_CHAR32,
    ENC_UTF8,
    ENC_WCHAR,
};

typedef struct Map {
    struct Map *parent;
    char **key;
    void **val;
    int size;
    int nelem;
    int nused;
} Map;

typedef struct {
    void **body;
    int len;
    int nalloc;
} Vector;

typedef struct {
    struct Map *map;
    Vector *key;
} Dict;

typedef struct Set {
    char *v;
    struct Set *next;
} Set;

typedef struct {
    char *body;
    int nalloc;
    int len;
} Buffer;

typedef struct {
    char *p;     // stream backed by string
    char *name;
    int line;
    int column;
    int ntok;     // token counter
    int last;     // the last character read from file
    int buf[3];   // push-back buffer for unread operations
    int buflen;   // push-back buffer size
} File;

typedef struct {
    int kind;
    File *file;
    int line;
    int column;
    boolean space;   // true if the token has a leading space
    boolean bol;     // true if the token is at the beginning of a line
    int count;    // token number in a file, counting from 0.
    Set *hideset; // used by the preprocessor for macro expansion
    union {
        // TKEYWORD
        int id;
        // TSTRING or TCHAR
        struct {
            char *sval;
            int slen;
            int c;
            int enc;
        };
        // TMACRO_PARAM
        struct {
            boolean is_vararg;
            int position;
        };
    };
} Token;

enum {
    AST_LITERAL = 256,
    AST_LVAR,
    AST_GVAR,
    AST_TYPEDEF,
    AST_FUNCALL,
    AST_FUNCPTR_CALL,
    AST_FUNCDESG,
    AST_FUNC,
    AST_DECL,
    AST_INIT,
    AST_CONV,
    AST_ADDR,
    AST_DEREF,
    AST_IF,
    AST_TERNARY,
    AST_DEFAULT,
    AST_RETURN,
    AST_COMPOUND_STMT,
    AST_STRUCT_REF,
    AST_GOTO,
    AST_COMPUTED_GOTO,
    AST_LABEL,
    OP_SIZEOF,
    OP_CAST,
    OP_SHR,
    OP_SHL,
    OP_A_SHR,
    OP_A_SHL,
    OP_PRE_INC,
    OP_PRE_DEC,
    OP_POST_INC,
    OP_POST_DEC,
    OP_LABEL_ADDR,
#define op(name, _) name,
#define keyword(name, x, y) name,
#include "keyword.inc"
#undef keyword
#undef op
};

enum {
    KIND_VOID,
    KIND_BOOLEAN,
    KIND_CHAR,
    KIND_SHORT,
    KIND_INT,
    KIND_LONG,
    KIND_LLONG,
    KIND_FLOAT,
    KIND_DOUBLE,
    KIND_LDOUBLE,
    KIND_ARRAY,
    KIND_ENUM,
    KIND_PTR,
    KIND_STRUCT,
    KIND_FUNC,
    // used only in parser
    KIND_STUB,
};

typedef struct Type {
    int kind;
    int size;
    int align;
    boolean usig; // true if unsigned
    boolean isstatic;
    // pointer or array
    struct Type *ptr;
    // array length
    int len;
    // struct
    Dict *fields;
    int offset;
    boolean is_struct; // true if struct, false if union
    // bitfield
    int bitoff;
    int bitsize;
    // function
    struct Type *rettype;
    Vector *params;
    boolean hasva;
    boolean oldstyle;
} Type;

typedef struct {
    char *file;
    int line;
} SourceLoc;

typedef struct Node {
    int kind;
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
            char *sval;
            char *slabel;
        };
        // Local/global variable
        struct {
            char *varname;
            // local
            int loff;
            Vector *lvarinit;
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
            char *fname;
            // Function call
            Vector *args;
            struct Type *ftype;
            // Function pointer or function designator
            struct Node *fptr;
            // Function declaration
            Vector *params;
            Vector *localvars;
            struct Node *body;
        };
        // Declaration
        struct {
            struct Node *declvar;
            Vector *declinit;
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
        Vector *stmts;
        // Struct reference
        struct {
            struct Node *struc;
            char *field;
            Type *fieldtype;
        };
    };
} Node;

extern Type *type_void;
extern Type *type_boolean;
extern Type *type_char;
extern Type *type_short;
extern Type *type_int;
extern Type *type_long;
extern Type *type_llong;
extern Type *type_uchar;
extern Type *type_ushort;
extern Type *type_uint;
extern Type *type_ulong;
extern Type *type_ullong;
extern Type *type_float;
extern Type *type_double;
extern Type *type_ldouble;

// debug.c
char *ty2s(Type *ty);
char *node2s(Node *node);
char *tok2s(Token *tok);

// error.c
extern boolean enable_warning;
extern boolean dumpstack;
extern boolean dumpsource;
extern boolean warning_is_error;

#define NULL ((void *)0)

#define STR2(x) #x
#define STR(x) STR2(x)
#define error(...)       errorf(__FILE__ ":" STR(__LINE__), NULL, __VA_ARGS__)
#define errort(tok, ...) errorf(__FILE__ ":" STR(__LINE__), token_pos(tok), __VA_ARGS__)
#define warn(...)        warnf(__FILE__ ":" STR(__LINE__), NULL, __VA_ARGS__)
#define warnt(tok, ...)  warnf(__FILE__ ":" STR(__LINE__), token_pos(tok), __VA_ARGS__)

void errorf(char *line, char *pos, char *fmt, ...);
void warnf(char *line, char *pos, char *fmt, ...);
char *token_pos(Token *tok);

// lex.c
void lex_init(char *filename);
char *get_base_file(void);
void skip_cond_incl(void);
char *read_header_file_name(boolean *std);
boolean is_keyword(Token *tok, int c);
void token_buffer_stash(Vector *buf);
void token_buffer_unstash();
void unget_token(Token *tok);
Token *lex_string(char *s);
Token *lex(void);

// parse.c
char *make_tempname(void);
char *make_label(void);
boolean is_inttype(Type *ty);
boolean is_flotype(Type *ty);
void *make_pair(void *first, void *second);
int eval_intexpr(Node *node, Node **addr);
Node *read_expr(void);
Vector *read_toplevels(void);
void parse_init(void);

