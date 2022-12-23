// paren object memory.

typedef union om *object;

#define ALIVE_BIT 0x80000000
#define HASH_MASK 0x0ffffff0
#define TYPE_MASK 0x0000000f
#define     SINT         0x0
#define     XINT         0x1
#define     XFLOAT       0x2
#define     CONS         0x3
#define     SYMBOL       0x4
#define     STRING       0x5
#define     BYTES        0x6
#define     ARRAY        0x7
#define     DICT         0x8
#define     MACRO        0x9
#define     FUNC         0xa
#define     SPECIAL      0xb
#define     BFUNC        0xc
#define     ENV          0xd

#define SINT_MAX 0x3fffffff
#define SINT_MIN (- SINT_MAX - 1)
#define XINT_BITS 63
#define DBL_MAX_INT ((int64_t)1 << DBL_MANT_DIG)
#define DBL_MIN_INT (- DBL_MAX_INT - 1)

#define LC(p) (*(unsigned char *)(p))
#define SC(p, v) (*(unsigned char *)(p) = (unsigned char)(v))

#define om_sint_p(o) ((((intptr_t)o) & 1) == 1)
#define om_sint_val(o) ((int)(((intptr_t)o) >> 1))
#define om_sint(i) ((object)((((intptr_t)i) << 1) | 1))
#define om_list_p(o) (o == om_nil || (om_type(o) == CONS))
#define om_keyword_p(o) (om_type(o) == SYMBOL && (o)->mem.elt[0] == ':')

union om {
  int header;
  struct xint {
    int header;
    int64_t val;
  } xint;
  struct xfloat {
    int header;
    double val;
  } xfloat;
  struct cons {
    int header;
    object car;
    object cdr;
  } cons;
  struct mem {
    int header;
    int size;
    char elt[1];
  } mem;
  struct array {
    int header;
    int size;
    object elt[1];
  } array;
  struct map {
    int header;
    int entry_count;
    int half_size;
    object top;
    object *table;
  } map;
  struct proc {
    int header;
    object env;
    int param_count;
    object params;
    object body;
  } proc;
  struct native {
    int header;
    object name;
    union {
      void *p;
      int (*special)(int, object);
      int (*function)(int, object, object *);
    } u;
  } native;
  object next;
};

// global object.
extern object om_toplevel;
extern object om_nil;
extern object om_true;
extern object om_key;
extern object om_opt;
extern object om_rest;
extern object om_quote;
extern object om_Class;
extern object om_class;
extern object om_symbol;
extern object om_super;
extern object om_features;
extern object om_fields;
extern object om_Exception;
extern object om_message;
extern object om_stack_trace;

enum Exception {
  Exception,
    SystemExit,
    Error,
      ArgumentError,
      ArithmeticError,
      StateError,
      OSError,
      SyntaxError
};

// constructor.
extern object om_new_xint(int64_t val);
extern object om_new_xfloat(double val);
extern object om_new_cons(object car, object cdr);
extern object om_copy_cons(object o, int size);
extern object om_new_bytes(int size);
extern object om_new_mem_from(int type, char *val, int size);
extern object om_new_mem_from_cstr(int type, char *cstr);
extern object om_new_mem_from_xbarray(int type, struct xbarray *x);
extern object om_new_cstring(object o);
extern object om_coerce_mem_string(object o);
extern object om_new_array(int size);
extern object om_new_array_from(object *o, int size);
extern object om_new_dict(void);
extern object om_new_func(object env, int param_count, object params, object body);
extern object om_new_macro(object env, int param_count, object params, object body);
extern object om_new_native(int type, object name, void *p);
extern object om_new_env(object top, int half_size);

// garbage collector.
extern void om_mark(object o);
extern void om_gc_chance(void);

// object manipulation.
extern int om_hash(object o);
extern int om_type(object o);
extern int om_eq_p(object o, object p);
extern int om_list_len(object o);
extern char *om_describe(object o, char *buf);
extern object om_bool(int b);
extern object om_list_reverse(object o);
extern object om_map_keys(object o);
extern object om_map_get(object o, object s);
extern object om_map_get_propagation(object o, object s);
extern void om_map_put(object o, object s, object v);
extern void om_map_put_propagation(object o, object s, object v);
extern void om_map_foreach(object o, void (*f)(void *s, void *v));

// initializer.
extern void om_init(void);
