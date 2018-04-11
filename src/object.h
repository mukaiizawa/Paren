// paren object

typedef union s_expr *object;

#define XINT_MAX 0x3fffffff
#define XINT_MIN (- XINT_MAX - 1)

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

extern char *object_type_name[];

struct object_header {
  enum object_type type;
  int hash, gen, alivep;
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
    int val; 
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

// table
extern struct xarray object_table;

// global object
extern object object_nil;
extern object object_true;
extern object object_false;

extern object object_alloc(void);
extern void object_dump(object o);

extern object object_bool(int b);

extern void object_init(void);
