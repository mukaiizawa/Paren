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

extern object object_nil;
extern object object_true;
extern object object_false;

extern int object_nilp(object o);
extern int object_consp(object o);
extern int object_listp(object o);

extern object object_type(object o);
extern object object_alloc();
extern object object_new_barray(int len);
extern object object_new_xint(int val);
extern object object_new_xfloat(double val);
extern void object_dump(object o);
