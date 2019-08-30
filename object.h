// paren object

typedef union s_expr *object;

#define ALIVE_MASK 0xf000
#define TYPE_MASK  0x0fff
#define   ENV      0x0001
#define   MACRO    0x0002
#define   LAMBDA   0x0004
#define   CONS     0x0008
#define   XINT     0x0010
#define   XFLOAT   0x0020
#define   SYMBOL   0x0040
#define   KEYWORD  0x0080
#define   STRING   0x0100
#define   BARRAY   0x0200
#define   ARRAY    0x0400

#define LINT_BITS 63

#define type(o) ((o)->header & TYPE_MASK)
#define typep(o, t) (type(o) == t)
#define listp(o) ((o) == object_nil || typep(o, CONS))
#define numberp(o) (typep(o, XINT) || typep(o, XFLOAT))
#define byte_range_p(x) ((x >= 0) && (x < 256))
#define bytep(o) (typep(o, XINT) && byte_range_p((o)->xint.val))

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
  struct barray {
    int header;
    int64_t size;
    char elt[1];
  } barray;
  struct array {
    int header;
    int64_t size;
    object elt[1];
  } array;
};

extern int symcmp(object o, object p);

extern struct xsplay special_splay;
extern struct xsplay prim_splay;

// global object
extern object object_boot;
extern object object_bytes[];
extern object object_catch;
extern object object_error;
extern object object_finally;
extern object object_key;
extern object object_nil;
extern object object_not;
extern object object_opt;
extern object object_os;
extern object object_quote;
extern object object_rest;
extern object object_st;
extern object object_toplevel;
extern object object_true;

extern int object_byte_size(object o);
extern char *object_describe(object o, char *buf);
extern int object_list_len(object o);
extern object object_bool(int b);
extern object object_reverse(object o);
