// paren object

typedef union s_expr *object;

#define ALIVE_MASK 0xf00000
#define TYPE_MASK 0x0fffff
#define   Lambda 0x1
#define   Cons 0x2
#define   Fbarray 0x3
#define   Farray 0x4
#define   Xint 0x5
#define   Xfloat 0x6
#define   Symbol 0x7
#define   Keyword 0x8

#define type(o) (o->header & TYPE_MASK)
#define typep(o, t) (type(o) == t)
#define listp(o) (o == object_nil || typep(o, Cons))

extern char *object_type_name[];

union s_expr {
  int header;
  struct lambda {
    int header;
    object top, params, body; 
    int prim_cd;
    struct xsplay binding;
  } lambda;
  struct cons {
    int header;
    object car, cdr; 
  } cons;
  struct fbarray {
    int header;
    int size;
    char elt[1];
  } fbarray;
  struct farray {
    int header;
    int size;
    object elt[1];
  } farray;
  struct xint {
    int header;
    int64_t val; 
  } xint;
  struct xfloat {
    int header;
    double val; 
  } xfloat;
  struct symbol {
    int header;
    char *name; 
  } symbol;
};

// global object
extern object toplevel;
extern object object_nil;
extern object object_true;
extern object object_false;
extern object object_opt;
extern object object_key;
extern object object_rest;

extern void object_dump(object o);
extern object object_bool(int b);
