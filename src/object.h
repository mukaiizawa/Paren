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

// global object
extern object object_nil;
extern object object_true;
extern object object_false;

// list utility
extern object CAR(object o);
extern object CDR(object o);
extern int TYPEP(object o, enum object_type type);
extern int NILP(object o);
extern int CONSP(object o);
extern int LISTP(object o);

extern void object_dump(object o);
