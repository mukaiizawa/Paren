// paren object

typedef union s_expr *object;

#define TYPE_MASK      0x0fffffff
#define ALIVE_MASK     0xf0000000
#define EVAL_INST_MASK 0x0f000000
#define BARRAY_MASK    0x00f00000
#define BUILTIN_MASK   0x000f0000
#define NUMBER_MASK    0x0000f000
#define   ENV          0x00000005
#define   MACRO        0x00000106
#define   LAMBDA       0x00000107
#define   SPECIAL      0x00010008
#define   FUNCITON     0x00010009
#define   CONS         0x01000010
#define   XINT         0x00001011
#define   XFLOAT       0x00001012
#define   SYMBOL       0x01100013
#define   KEYWORD      0x00100014
#define   STRING       0x00100015
#define   BARRAY       0x00100016
#define   ARRAY        0x00000017

#define LINT_BITS 63

#define type(o) ((o)->header & TYPE_MASK)
#define type_p(o, t) (type(o) == t)
#define list_p(o) ((o) == object_nil || type_p(o, CONS))
#define barray_p(o) ((o)->header & BARRAY_MASK)
#define number_p(o) ((o)->header & NUMBER_MASK)
#define builtin_p(o) ((o)->header & BUILTIN_MASK)
#define byte_range_p(x) ((x >= 0) && (x < 256))
#define byte_p(o) (type_p(o, XINT) && byte_range_p((o)->xint.val))

union s_expr {
  int header;
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
  void *p;
  int (*cmp)(object p, object q);
};

extern object object_symbol_splay;
extern object object_keyword_splay;
extern object object_symcmp;
extern object object_strcmp;

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
