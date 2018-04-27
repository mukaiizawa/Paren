// paren object

typedef union s_expr *object;

#define ALIVE_MASK 0xf000
#define TYPE_MASK  0x0fff
#define   Env      0x0001
#define   Lambda   0x0002
#define   Cons     0x0004
#define   Fbarray  0x0008
#define   Farray   0x0010
#define   Xint     0x0020
#define   Xfloat   0x0040
#define   Symbol   0x0080
#define   Keyword  0x0100

#define type(o) (o->header & TYPE_MASK)
#define typep(o, t) (type(o) == t)
#define listp(o) (o == object_nil || typep(o, Cons))

extern char *object_type_name[];

union s_expr {
  int header;
  struct env {
    int header;
    object top;
    struct xsplay binding;
  } env;
  struct lambda {
    int header;
    object env, params, body;
    int prim_cd;
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

extern int symcmp(object o, object p);

// global object
extern object object_toplevel;
extern object object_nil;
extern object object_true;
extern object object_false;
extern object object_opt;
extern object object_key;
extern object object_rest;

extern object object_if;
extern object object_quote;
extern object object_assign;
extern object object_lambda;

extern void object_dump(object o);
extern object object_bool(int b);
