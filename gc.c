// garbage collector

#include "std.h"
#include "xarray.h"
#include "heap.h"
#include "splay.h"
#include "object.h"
#include "st.h"
#include "ip.h"
#include "gc.h"

int gc_used_memory;
int gc_max_used_memory;

#define LINK0_SIZE (sizeof(struct cons))
#define LINK1_SIZE (2 * LINK0_SIZE)
static object link0, link1;

static struct heap heap;
static struct xarray *table, *work_table, table0, table1;
static struct st symbol_table;
static struct st keyword_table;

#define regist(o) (xarray_add(table, o))

static object gc_alloc(int size)
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
  o->header &= 0;
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
  splay_init(&o->env.binding);
  regist(o);
  return o;
}

static object new_lambda(int type, object env, object params, object body)
{
  object o;
  xassert(object_type_p(env, ENV));
  o = gc_alloc(sizeof(struct lambda));
  o->lambda.env = env;
  o->lambda.params = params;
  o->lambda.body = body;
  set_type(o, type);
  regist(o);
  return o;
}

object gc_new_macro(object env, object params, object body)
{
  return new_lambda(MACRO, env, params, body);
}

object gc_new_lambda(object env, object params, object body)
{
  return new_lambda(LAMBDA, env, params, body);
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

object gc_new_xint(int64_t val)
{
  object o;
  if (SINT_MIN <= val && val <= SINT_MAX) return sint((int)val);
  o = gc_alloc(sizeof(struct xint));
  set_type(o, XINT);
  o->xint.val = val;
  regist(o);
  return o;
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

static object new_cons(void)
{
  object o;
  o = gc_alloc(sizeof(struct cons));
  set_type(o, CONS);
  regist(o);
  return o;
}

object gc_new_cons(object car, object cdr)
{
  object o;
  o = new_cons();
  o->cons.car = car;
  o->cons.cdr = cdr;
  return o;
}

object gc_copy_cons(object o, object *tail)
{
  object head;
  if (o == object_nil) return object_nil;
  head = *tail = new_cons();
  (*tail)->cons.car = o->cons.car;
  while ((o = o->cons.cdr) != object_nil) {
    *tail = (*tail)->cons.cdr = new_cons();
    (*tail)->cons.car = o->cons.car;
  }
  (*tail)->cons.cdr = object_nil;
  return head;
}

static object new_bytes(int type, int size)
{
  object o;
  xassert(size >= 0);
  o = gc_alloc(sizeof(struct bytes) + size - 1);
  set_type(o, type);
  o->bytes.size = size;
  regist(o);
  return o;
}

object gc_new_bytes(int type, int size)
{
  object o;
  o = new_bytes(type, size);
  memset(o->bytes.elt, 0, size);
  return o;
}

static object new_bytes_from(int type, char *val, int size)
{
  object o;
  o = new_bytes(type, size);
  memcpy(o->bytes.elt, val, size);
  return o;
}

object gc_new_bytes_from(int type, char *val, int size)
{
  object o;
  struct st *s;
  switch (type) {
    case SYMBOL:
    case KEYWORD:
      if (type == SYMBOL) s = &symbol_table;
      else s = &keyword_table;
      if ((o = st_get(s, val, size)) != NULL) return o;
      return st_put(s, new_bytes_from(type, val, size));
    case STRING:
    case BYTES:
      return new_bytes_from(type, val, size);
    default:
      xassert(FALSE);
      return NULL;
  }
}

static object new_array(int size)
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

object gc_new_Error(char *msg)
{
  return gc_new_cons(object_class
      , gc_new_cons(object_Error
        , gc_new_cons(object_message
          , gc_new_cons(gc_new_bytes_from(STRING, msg, strlen(msg))
            , gc_new_cons(object_stack_trace
              , gc_new_cons(object_nil , object_nil))))));
}

static void mark_binding(void *key, void *data)
{
  gc_mark(key);
  gc_mark(data);
}

void gc_mark(object o)
{
  int i;
  object p;
  if (sint_p(o)) return;
  if (alive_p(o)) return;
  set_alive(o);
  switch (object_type(o)) {
    case ENV:
      splay_foreach(&o->env.binding, mark_binding);
      gc_mark(o->env.top);
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
      p = o;
      while (o != object_nil) {
        o = o->cons.cdr;
        set_alive(o);    // for stack overflow
      }
      while (p != object_nil) {
        gc_mark(p->cons.car);
        p = p->cons.cdr;
      }
      break;
    case ARRAY:
      for (i = 0; i < o->array.size; i++)
        gc_mark(o->array.elt[i]);
      break;
    default: break;
  }
}

static void gc_free(object o)
{
  int size;
  size = object_byte_size(o);
  if (object_type_p(o, ENV)) splay_free(&o->env.binding);
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

static void sweep_s_expr(void)
{
  int i;
  object o;
  struct xarray *p;
  xarray_reset(work_table);
  st_reset(&symbol_table);
  st_reset(&keyword_table);
  for (i = 0; i < (*table).size; i++) {
    o = (*table).elt[i];
    if (alive_p(o)) {
      switch (object_type(o)) {
        case SYMBOL: st_put(&symbol_table, o); break;
        case KEYWORD: st_put(&keyword_table, o); break;
        default: break;
      }
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
  ip_mark_object();
  sweep_s_expr();
  if (GC_LOG_P) printf("after gc(used memory %d[byte])\n", gc_used_memory);
}

void gc_init(void)
{
  gc_used_memory = gc_max_used_memory = 0;
  heap_init(&heap);
  link0 = link1 = NULL;
  xarray_init(&table0);
  xarray_init(&table1);
  table = &table0;
  work_table = &table1;
  st_init(&symbol_table);
  st_init(&keyword_table);
}
