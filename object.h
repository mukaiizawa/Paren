// paren object

typedef union s_expr *object;

#define HASH_MASK      0x00ffffff
#define TYPE_MASK      0x00000fff
#define ALIVE_BIT      0x00001000
#define EVAL_FRAME_BIT 0x00000100
#define BARRAY_BIT     0x00000200
#define BUILTIN_BIT    0x00000400
#define NUMBER_BIT     0x00000800
#define   SPLAY        0x00000001
#define   ENV          0x00000002
#define   MACRO        0x00000003
#define   LAMBDA       0x00000004
#define   SPECIAL     (0x00000005 | BUILTIN_BIT)
#define   FUNCITON    (0x00000006 | BUILTIN_BIT)
#define   CONS        (0x00000007 | EVAL_FRAME_BIT)
#define   XINT        (0x00000008 | NUMBER_BIT)
#define   XFLOAT      (0x00000009 | NUMBER_BIT)
#define   SYMBOL      (0x0000000a | BARRAY_BIT | EVAL_FRAME_BIT)
#define   KEYWORD     (0x0000000b | BARRAY_BIT)
#define   STRING      (0x0000000c | BARRAY_BIT)
#define   BARRAY      (0x0000000d | BARRAY_BIT)
#define   ARRAY        0x0000000e

#define LINT_BITS 63

#define type(o) ((o)->header & TYPE_MASK)
#define type_p(o, t) (type(o) == t)
#define list_p(o) ((o) == object_nil || type_p(o, CONS))
#define barray_p(o) ((o)->header & BARRAY_BIT)
#define number_p(o) ((o)->header & NUMBER_BIT)
#define builtin_p(o) ((o)->header & BUILTIN_BIT)
#define byte_range_p(x) ((x >= 0) && (x < 256))
#define byte_p(o) (type_p(o, XINT) && byte_range_p((o)->xint.val))

union s_expr {
  int header;
  struct splay {
    int header;
    object top;
  } splay;
  struct env {
    int header;
    object top, binding;
  } env;
  struct lambda {
    int header;
    object env, params, body;
  } lambda;
  struct builtin {
    int header;
    object name;
    union {
      void *p;
      int (*special)(int, object);
      int (*function)(int, object, object *);
    } u;
  } builtin;
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
    int size;
    char elt[1];
  } barray;
  struct array {
    int header;
    int size;
    object elt[1];
  } array;
  object next;
};

// global object
extern object object_nil;
extern object object_true;
extern object object_key;
extern object object_opt;
extern object object_rest;
extern object object_quote;
extern object object_bytes[];
extern object object_toplevel;
extern object object_stack_trace;
extern object object_boot;
extern object object_splay_nil;

extern object object_Class;
extern object object_Exception;
extern object object_Error;

extern object object_class;
extern object object_symbol;
extern object object_super;
extern object object_features;
extern object object_fields;
extern object object_message;

extern int object_byte_size(object o);
extern char *object_describe(object o, char *buf);
extern int object_list_len(object o);
extern object object_bool(int b);
extern object object_reverse(object o);
