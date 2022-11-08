// paren object.

typedef union _object *object;

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

#define SINT_BITS 30
#define SINT_MAX 0x3fffffff
#define SINT_MIN (- SINT_MAX - 1)
#define XINT_BITS 63
#define DBL_MAX_INT ((int64_t)1 << DBL_MANT_DIG)
#define DBL_MIN_INT (- DBL_MAX_INT - 1)

#define LC(p) (*(unsigned char *)(p))
#define SC(p,v) (*(unsigned char *)(p) = (unsigned char)(v))

union _object {
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

extern object object_toplevel;
extern object object_nil;
extern object object_true;
extern object object_key;
extern object object_opt;
extern object object_rest;
extern object object_quote;

// POS
extern object object_Class;
extern object object_class;
extern object object_symbol;
extern object object_super;
extern object object_features;
extern object object_fields;
extern object object_Exception;
extern object object_message;
extern object object_stack_trace;

extern void object_set_alive(object o);
extern void object_set_dead(object o);
extern int object_alive_p(object o);
extern int object_hash(object o);
extern int object_mem_hash(char *p, int size);
extern int object_number_hash(double val);
extern void object_set_hash(object o, int hval);
extern int object_type(object o);
extern void object_set_type(object o, int type);
extern void object_reset_type(object o, int type);

extern int object_byte_size(object o);
extern char *object_describe(object o, char *buf);
extern object object_bool(int b);
extern int object_eq_p(object o, object p);

#define sint_p(o) ((((intptr_t)o) & 1) == 1)
#define sint_val(o) ((int)(((intptr_t)o) >> 1))
#define sint(i) ((object)((((intptr_t)i) << 1) | 1))
#define list_p(o) (o == object_nil || (object_type(o) == CONS))
#define keyword_p(o) (object_type(o) == SYMBOL && (o)->mem.elt[0] == ':')

extern int list_len(object o);
extern object list_reverse(object o);

extern int ch_at(object o, int *i, object *result);
extern int ch_len(unsigned char ch, int *len);
extern int str_len(object o, int *len);

extern object map_get(object o, object s);
extern object map_get_propagation(object o, object s);
extern void map_put(object o, object s, object v);
extern void map_put_propagation(object o, object s, object v);
extern object map_keys(object o);
extern void map_foreach(object o, void (*f)(void *s, void *v));
