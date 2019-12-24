// paren object

typedef union s_expr *object;

#define TYPE_MASK      0x0fffffff
#define ALIVE_MASK     0xf0000000
#define EVAL_INST_MASK 0x0f000000
#define BYTE_SEQ_MASK  0x00f00000
#define GC_TARGET_MASK 0x0000f000
#define   ENV          0x00000005
#define   MACRO        0x00001006
#define   LAMBDA       0x00001007
#define   SPECIAL      0x00000008
#define   FUNCITON     0x00000009
#define   CONS         0x01001010
#define   XINT         0x00001011
#define   XFLOAT       0x00001012
#define   SYMBOL       0x01111013
#define   KEYWORD      0x00111014
#define   STRING       0x00111015
#define   BARRAY       0x00111016
#define   ARRAY        0x00011017

#define LINT_BITS 63

#define type(o) ((o)->header & TYPE_MASK)
#define typep(o, t) (type(o) == t)
#define listp(o) ((o) == object_nil || typep(o, CONS))
#define byte_seqp(o) ((o)->header & BYTE_SEQ_MASK)
#define gc_targetp(o) ((o)->header & GC_TARGET_MASK)
#define numberp(o) (typep(o, XINT) || typep(o, XFLOAT))
#define byte_range_p(x) ((x >= 0) && (x < 256))
#define bytep(o) (typep(o, XINT) && byte_range_p((o)->xint.val))

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
extern object object_class;
extern object object_message;
extern object object_stack_trace;
extern object object_Error;
extern object object_Exception;
extern object object_boot;
extern object object_splay_nil;

extern int object_byte_size(object o);
extern char *object_describe(object o, char *buf);
extern int object_list_len(object o);
extern object object_bool(int b);
extern object object_reverse(object o);
