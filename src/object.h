// paren object

typedef union s_expr *object;

enum object_type {
  cons,
  xint,
  xfloat,
  symbol,
  keyword
};

struct object_header {
  enum object_type type;
} object_header;

union s_expr {
  struct object_header header;
  struct cons {
    struct object_header header;
    union s_expr *car, *cdr; 
  } cons;
  struct xint {
    struct object_header header;
    int64_t val; 
  } xint;
  struct xfloat {
    struct object_header header;
    double val; 
  } xfloat;
  struct symbol {
    struct object_header header;
    char *name; 
    union s_expr *val; 
  } symbol;
  struct keyword {
    struct object_header header;
    char *name; 
  } keyword;
};

extern object object_nil;
extern object object_true;
extern object object_false;

extern int object_nilp(object o);
extern int object_consp(object o);
extern int object_listp(object o);

extern object object_alloc(int size);
extern object object_type(object o);
extern object object_new_cons(object car, object cdr);
extern object object_new_symbol(char *name);
extern object object_new_keyword(char *name);
extern void object_dump(object o);
