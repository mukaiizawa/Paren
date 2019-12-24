// garbage collector

#include "std.h"
#include "xarray.h"
#include "heap.h"
#include "object.h"
#include "splay.h"
#include "ip.h"
#include "gc.h"

int gc_used_memory, gc_max_used_memory;

#define LINK0_SIZE (sizeof(struct cons))
#define LINK1_SIZE (2 * LINK0_SIZE)
static object link0, link1;

static struct heap heap;
static struct xarray *table, *work_table, table0, table1;

#define alivep(o) ((o)->header & ALIVE_MASK)
#define set_alive(o) ((o)->header |= ALIVE_MASK)
#define set_dead(o) ((o)->header &= ~ALIVE_MASK)
#define set_type(o, type) {(o)->header &= ~TYPE_MASK; (o)->header |= type;}
#define regist(o) (xarray_add(table, o))

STATIC object gc_alloc(int size)
{
  object o;
  if (size <= LINK0_SIZE) {
    size = LINK0_SIZE;
    if (link0 == NULL) o = heap_alloc(&heap, size);
    else {
      o = link0;
      link0 = o->next;
    }
  } else if (size <= LINK1_SIZE) {
    size = LINK1_SIZE;
    if (link1 == NULL) o = heap_alloc(&heap, size);
    else {
      o = link1;
      link1 = o->next;
    }
  } else o = xmalloc(size);
  set_dead(o);
  gc_used_memory += size;
  if (gc_used_memory > gc_max_used_memory) gc_max_used_memory = gc_used_memory;
  return o;
}

object gc_new_env(object top)
{
  object o;
  o = gc_alloc(sizeof(struct env));
  set_type(o, ENV);
  o->env.top = top;
  o->env.binding = gc_new_splay(object_symcmp);
  regist(o);
  return o;
}

STATIC object new_lambda(object env, object params, object body)
{
  object o;
  xassert(typep(env, ENV));
  o = gc_alloc(sizeof(struct lambda));
  o->lambda.env = env;
  o->lambda.params = params;
  o->lambda.body = body;
  regist(o);
  return o;
}

object gc_new_macro(object env, object params, object body)
{
  object o;
  o = new_lambda(env, params, body);
  set_type(o, MACRO);
  return o;
}

object gc_new_lambda(object env, object params, object body)
{
  object o;
  o = new_lambda(env, params, body);
  set_type(o, LAMBDA);
  return o;
}

object gc_new_builtin(int type, object name, void *p)
{
  object o;
  o = gc_alloc(sizeof(struct builtin));
  o->builtin.name = name;
  o->builtin.u.p = p;
  set_type(o, type);
  regist(o);
  return o;
}

object gc_new_bytes(int64_t val)
{
  object o;
  o = gc_alloc(sizeof(struct xint));
  set_type(o, XINT);
  o->xint.val = val;
  regist(o);
  return o;
}

object gc_new_xint(int64_t val)
{
  if (byte_range_p(val)) return object_bytes[val];
  return gc_new_bytes(val);
}

object gc_new_xfloat(double val)
{
  object o;
  o = gc_alloc(sizeof(struct xfloat));
  set_type(o, XFLOAT);
  o->xfloat.val = val;
  regist(o);
  return o;
}

object gc_new_cons(object car, object cdr)
{
  object o;
  o = gc_alloc(sizeof(struct cons));
  set_type(o, CONS);
  o->cons.car = car;
  o->cons.cdr = cdr;
  regist(o);
  return o;
}

object gc_new_barray(int type, int size)
{
  object o;
  xassert(size >= 0);
  o = gc_alloc(sizeof(struct barray) + size - 1);
  set_type(o, type);
  memset(o->barray.elt, 0, size);
  o->barray.size = size;
  regist(o);
  return o;
}

STATIC object new_barray_from(int type, char *val, int size)
{
  object o;
  o = gc_new_barray(type, size);
  memcpy(o->barray.elt, val, size);
  return o;
}

object gc_new_barray_from(int type, char *val, int size)
{
  object o, p;
  o = new_barray_from(type, val, size);
  switch (type) {
    case SYMBOL:
      if ((p = splay_find(object_symbol_splay, o)) != NULL) return p;
      splay_add(object_symbol_splay, o, o);
      return o;
    case KEYWORD:
      if ((p = splay_find(object_keyword_splay, o)) != NULL) return p;
      splay_add(object_keyword_splay, o, o);
      return o;
    case STRING:
    case BARRAY:
      return o;
    default:
      xassert(FALSE);
      return NULL;
  }
}

STATIC object new_array(int size)
{
  object o;
  xassert(size >= 0);
  o = gc_alloc(sizeof(struct array) + sizeof(object) * (size - 1));
  set_type(o, ARRAY);
  o->array.size = size;
  regist(o);
  return o;
}

object gc_new_array(int size)
{
  int i;
  object o;
  o = new_array(size);
  for (i = 0; i < size; i++) o->array.elt[i] = object_nil;
  return o;
}

object gc_new_array_from(object *o, int size)
{
  object p;
  p = new_array(size);
  memcpy(p->array.elt, o, size * sizeof(object));
  return p;
}

object gc_new_pointer(void *p)
{
  object o;
  o = gc_alloc(sizeof(void *));
  o->p = p;
  regist(o);
  return o;
}

object gc_new_splay(object cmp)
{
  return gc_new_cons(object_splay_nil, cmp);
}

object gc_new_splay_node(object k, object v, object l, object r)
{
  object o;
  o = gc_new_array(4);
  o->array.elt[0] = k;
  o->array.elt[1] = v;
  o->array.elt[2] = l;
  o->array.elt[3] = r;
  return o;
}

object gc_new_throwable(object e, char *msg)
{
  return gc_new_cons(object_class, gc_new_cons(e, gc_new_cons(object_message
          , gc_new_cons(gc_new_barray_from(STRING, msg, strlen(msg))
            , object_nil))));
}

void gc_mark(object o)
{
  int i;
  if (alivep(o)) return;
  set_alive(o);
  switch (type(o)) {
    case ENV:
      gc_mark(o->env.top);
      gc_mark(o->env.binding);
      break;
    case SPECIAL:
    case FUNCITON:
      gc_mark(o->builtin.name);
      break;
    case MACRO:
    case LAMBDA:
      gc_mark(o->lambda.env);
      gc_mark(o->lambda.params);
      gc_mark(o->lambda.body);
      break;
    case CONS:
      gc_mark(o->cons.car);
      gc_mark(o->cons.cdr);
      break;
    case ARRAY:
      for (i = 0; i < o->array.size; i++) gc_mark(o->array.elt[i]);
      break;
    default: xassert(gc_targetp(o)); break;
  }
}

STATIC void gc_free(object o)
{
  int size;
  size = object_byte_size(o);
  if (size <= LINK0_SIZE) {
    size = LINK0_SIZE;
    o->next = link0;
    link0 = o;
  } else if (size <= LINK1_SIZE) {
    size = LINK1_SIZE;
    o->next = link1;
    link1 = o;
  } else xfree(o);
  gc_used_memory -= size;
}

STATIC void sweep_s_expr(void)
{
  int i;
  object o;
  struct xarray *p;
  xarray_reset(work_table);
  for (i = 0; i < (*table).size; i++) {
    o = (*table).elt[i];
    if (alivep(o)) {
      set_dead(o);
      xarray_add(work_table, o);
    } else gc_free(o);
  }
  p = work_table;
  work_table = table;
  table = p;
}

void gc_chance(void)
{
  if (gc_used_memory < GC_CHANCE_MEMORY) return;
  if (GC_LOG_P) printf("before gc(used memory %d[byte])\n", gc_used_memory);
  ip_mark();
  gc_mark(object_symcmp);
  gc_mark(object_strcmp);
  gc_mark(object_symbol_splay);
  gc_mark(object_keyword_splay);
  sweep_s_expr();
  if (GC_LOG_P) printf("after gc(used memory %d[byte])\n", gc_used_memory);
}

void gc_init(void)
{
  gc_used_memory = gc_max_used_memory = 0;
  link0 = link1 = NULL;
  xarray_init(&table0);
  xarray_init(&table1);
  table = &table0;
  work_table = &table1;
}
