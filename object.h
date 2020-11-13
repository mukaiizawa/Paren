// paren object

typedef union s_expr *object;

#define HASH_MASK      0x000fffff
#define ALIVE_BIT      0x01000000
#define TYPE_MASK      0x00f00000
#define   ENV          0x00000001
#define   MACRO        0x00000002
#define   FUNC         0x00000003
#define   SPECIAL      0x00000004
#define   BUILTINFUNC  0x00000005
#define   CONS         0x00000006
#define   SINT         0x00000007
#define   XINT         0x00000008
#define   XFLOAT       0x00000009
#define   SYMBOL       0x0000000a
#define   KEYWORD      0x0000000b
#define   STRING       0x0000000c
#define   BYTES        0x0000000d
#define   ARRAY        0x0000000e

#define TYPE_POS 20

#define SINT_BITS 30
#define SINT_MAX 0x3fffffff
#define SINT_MIN (-SINT_MAX-1)

#define XINT_BITS 63

#define LC(p) (*(unsigned char*)(p))
#define SC(p,v) (*(unsigned char*)(p)=(unsigned char)(v))

#define hash(o) ((o)->header & HASH_MASK)
#define set_hash(o, v) {(o)->header &= ~HASH_MASK; (o)->header |= v;}

#define set_alive(o) ((o)->header |= ALIVE_BIT)
#define set_dead(o) ((o)->header &= ~ALIVE_BIT)
#define alive_p(o) ((o)->header & ALIVE_BIT)

#define set_type(o, type) {(o)->header &= ~TYPE_MASK; (o)->header |= (type << TYPE_POS);}
#define object_type(o) (sint_p(o)? SINT: (o->header & TYPE_MASK)>> TYPE_POS)
#define object_type_p(o, type) (object_type(o) == type)

#define byte_p(i) (0 <= i && i < 256)
#define sint_p(o) ((((intptr_t)o) & 1) == 1)
#define sint_val(o) ((int)(((intptr_t)o) >> 1))
#define sint(i) ((object)((((intptr_t)i) << 1) | 1))

union s_expr {
  int header;
  struct env {
    int header;
    int symbol_count;
    int half_size;
    object top;
    object *table;
    // object[half_size] syms;
    // object[half_size] vals;
  } env;
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
  struct cons {
    int header;
    object car;
    object cdr;
  } cons;
  struct xint {
    int header;
    int64_t val;
  } xint;
  struct xfloat {
    int header;
    double val; 
  } xfloat;
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
  object next;
};

extern object object_toplevel;
extern object object_nil;
extern object object_true;
extern object object_key;
extern object object_opt;
extern object object_rest;
extern object object_quote;
extern object object_stack_trace;

extern object object_Class;
extern object object_Exception;
extern object object_Error;

extern object object_class;
extern object object_symbol;
extern object object_super;
extern object object_features;
extern object object_fields;
extern object object_message;

extern int object_list_p(object o);
extern int object_byte_size(object o);
extern char *object_describe(object o, char *buf);
extern int object_list_len(object o);
extern object object_bool(int b);
extern object object_reverse(object o);

extern object object_find(object e, object s);
extern object object_find_propagation(object e, object s);
extern void object_bind(object e, object s, object v);
extern void object_bind_propagation(object e, object s, object v);
extern void object_env_foreach(object e, void (*f)(void *s, void *v));
