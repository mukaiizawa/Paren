// paren object.

typedef union _object *object;

#define ALIVE_BIT 0x01000000
#define TYPE_MASK 0x00f00000
#define HASH_MASK 0x000fffff

#define TYPE_OFFSET 20
#define SINT        0x0
#define XINT        0x1
#define XFLOAT      0x2
#define CONS        0x3
#define SYMBOL      0x4
#define KEYWORD     0x5
#define STRING      0x6
#define BYTES       0x7
#define ARRAY       0x8
#define DICT        0x9
#define MACRO       0xa
#define FUNC        0xb
#define SPECIAL     0xc
#define BUILTINFUNC 0xd
#define ENV         0xe

#define SINT_BITS 30
#define SINT_MAX 0x3fffffff
#define SINT_MIN (- SINT_MAX - 1)
#define XINT_BITS 63
#define DBL_MAX_INT ((int64_t)1 << DBL_MANT_DIG)
#define DBL_MIN_INT (- DBL_MAX_INT - 1)

#define LC(p) (*(unsigned char *)(p))
#define SC(p,v) (*(unsigned char *)(p) = (unsigned char)(v))

#define object_hash(o) ((o)->header & HASH_MASK)
#define object_set_hash(o, v) {(o)->header &= ~HASH_MASK; (o)->header |= v;}

#define object_set_alive(o) ((o)->header |= ALIVE_BIT)
#define object_set_dead(o) ((o)->header &= ~ALIVE_BIT)
#define object_alive_p(o) ((o)->header & ALIVE_BIT)

#define object_set_type(o, type) {(o)->header &= ~TYPE_MASK; (o)->header |= (type << TYPE_OFFSET);}
#define object_type(o) (sint_p(o)? SINT: (o->header & TYPE_MASK)>> TYPE_OFFSET)
#define object_type_p(o, type) (object_type(o) == type)

#define sint_p(o) ((((intptr_t)o) & 1) == 1)
#define sint_val(o) ((int)(((intptr_t)o) >> 1))
#define sint(i) ((object)((((intptr_t)i) << 1) | 1))
#define list_p(o) (o == object_nil || object_type_p(o, CONS))

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
  struct builtin {
    int header;
    object name;
    union {
      void *p;
      int (*special)(int, object);
      int (*function)(int, object, object *);
    } u;
  } builtin;
  object next;
};

// symbol & keyword.
extern object object_toplevel;
extern object object_nil;
extern object object_true;
extern object object_key;
extern object object_opt;
extern object object_rest;
extern object object_quote;
extern object object_stack_trace;

// paren object system.
extern object object_Class;
extern object object_Exception;
extern object object_class;
extern object object_symbol;
extern object object_super;
extern object object_features;
extern object object_fields;
extern object object_message;

// utility functions.
extern int object_byte_size(object o);
extern char *object_describe(object o, char *buf);
extern object object_bool(int b);
extern int object_eq_p(object o, object p);

extern int int_p(object o);
extern int number_p(object o);
extern int bytes_like_p(object o);
extern int sequence_p(object o);
extern int mutable_sequence_p(object o);
extern int collection_p(object o);
extern int symbol_keyword_p(object o);

extern int list_len(object o);
extern object list_reverse(object o);

extern int ch_len(unsigned char ch, int *len);
extern int str_len(object o, int *len);

extern object map_get(object o, object s);
extern object map_get_propagation(object o, object s);
extern void map_put(object o, object s, object v);
extern void map_put_propagation(object o, object s, object v);
extern object map_keys(object o);
extern void map_foreach(object o, void (*f)(void *s, void *v));
