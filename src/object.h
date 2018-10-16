// paren object

typedef union s_expr *object;

#define ALIVE_MASK 0xf000
#define TYPE_MASK  0x0fff
#define   Env      0x0001
#define   Macro    0x0002
#define   Lambda   0x0004
#define   Cons     0x0008
#define   Xint     0x0010
#define   Xfloat   0x0020
#define   Symbol   0x0040
#define   Keyword  0x0080

#define XINT_MAX 0x3fffffff
#define XINT_MIN (- XINT_MAX - 1)
#define SINT_MAX 255

#define type(o) ((o)->header & TYPE_MASK)
#define typep(o, t) (type(o) == t)
#define listp(o) (o == object_nil || typep(o, Cons))

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
  } lambda;
  struct cons {
    int header;
    object car, cdr; 
  } cons;
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
extern object object_boot;
extern object object_nil;
extern object object_true;
extern object object_opt;
extern object object_key;
extern object object_rest;
extern object object_quote;
extern object object_bq;
extern object object_uq;
extern object object_splice;
extern object object_not;
extern object object_sint[SINT_MAX];

extern char *object_describe(object o, char *buf);
extern int object_length(object o);
extern object object_bool(int b);
extern object object_nth(object o, int n);
extern object object_reverse(object o);
