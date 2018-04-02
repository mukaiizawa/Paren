// paren object

typedef union s_expr *object;

enum object_type {
  lambda,
  cons,
  fbarray,
  farray,
  xint,
  xfloat,
  symbol,
  keyword
};

struct object_header {
  enum object_type type;
  int hash;
} object_header;

union s_expr {
  struct object_header header;
  struct lambda {
    struct object_header header;
    object top, params, body; 
    int prim_cd;
    struct xsplay binding;
  } lambda;
  struct cons {
    struct object_header header;
    object car, cdr; 
  } cons;
  struct fbarray {
    struct object_header header;
    int size;
    char elt[1];
  } fbarray;
  struct farray {
    struct object_header header;
    int size;
    object elt[1];
  } farray;
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
  } symbol;
};

// global object
extern object object_nil;
extern object object_true;
extern object object_false;
extern object object_prim;
extern object object_error;
extern object object_pre_condition_error;
extern object object_post_condition_error;
extern object object_argument_error;

// list utility
extern object object_car(object o);
extern object object_cdr(object o);
extern object object_nth(object o, int n);
extern int object_length(object o);
extern int object_typep(object o, enum object_type type);
extern int object_nilp(object o);
extern int object_consp(object o);
extern int object_listp(object o);
extern int object_bool(object o);

extern void object_dump(object o);
