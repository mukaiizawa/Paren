// garbage collector.

#include "std.h"
#include "heap.h"
#include "xarray.h"
#include "object.h"
#include "st.h"
#include "ip.h"
#include "gc.h"

int gc_used_memory;
int gc_max_used_memory;

#define LINK0_SIZE (sizeof(struct cons))
#define LINK1_SIZE (sizeof(struct map))
static object link0, link1;

static struct heap heap;
static struct xarray *table, *work_table, table0, table1;
static struct st symbol_table;
static struct st keyword_table;

#define regist(o) (xarray_add(table, o))

static int mem_hash(char *val, int size)
{
  int i, hval;
  for (i = hval = 0; i < size; i++) hval = hval * 137 + LC(val + i);
  return hval & HASH_MASK;
}

static void *gc_alloc(int size)
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
  gc_used_memory += size;
  if (gc_used_memory > gc_max_used_memory) gc_max_used_memory = gc_used_memory;
  o->header &= 0;
  return o;
}

static void gc_free0(int size, void *p)
{
  object o;
  o = p;
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

void gc_free(object o)
{
  switch (object_type(o)) {
    case DICT:
    case ENV:
      gc_free0(sizeof(object) * o->map.half_size * 2, o->map.table);
      break;
    default:
      break;
  }
  gc_free0(object_byte_size(o), o);
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

static object new_mem(int type, int size)
{
  object o;
  xassert(size >= 0);
  o = gc_alloc(sizeof(struct mem) + size - 1);
  set_type(o, type);
  o->mem.size = size;
  regist(o);
  return o;
}

object gc_new_mem(int type, int size)
{
  object o;
  o = new_mem(type, size);
  memset(o->mem.elt, 0, size);
  return o;
}

static object new_mem_from(int type, char *val, int size)
{
  object o;
  o = new_mem(type, size);
  memcpy(o->mem.elt, val, size);
  return o;
}

object gc_new_mem_from(int type, char *val, int size)
{
  int hval;
  object o;
  struct st *s;
  switch (type) {
    case SYMBOL:
    case KEYWORD:
      if (type == SYMBOL) s = &symbol_table;
      else s = &keyword_table;
      hval = mem_hash(val, size);
      if ((o = st_get(s, val, size, hval)) != NULL) return o;
      o = new_mem_from(type, val, size);
      set_hash(o, hval);
      return st_put(s, o);
    case STRING:
    case BYTES:
      return new_mem_from(type, val, size);
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

static object new_map(int type, int half_size, object top)
{
  int i;
  object o;
  o = gc_alloc(sizeof(struct map));
  set_type(o, type);
  o->map.top = top;
  o->map.entry_count = 0;
  o->map.half_size = half_size;
  o->map.table = gc_alloc(sizeof(object) * half_size * 2);
  for (i = 0; i < half_size; i++) o->map.table[i] = NULL;
  regist(o);
  return o;
}

object gc_new_dict(void)
{
  return new_map(DICT, 8, object_nil);
}

static object new_proc(int type, object env, int param_count, object params
    , object body)
{
  object o;
  xassert(object_type_p(env, ENV));
  o = gc_alloc(sizeof(struct proc));
  o->proc.env = env;
  o->proc.param_count = param_count;
  o->proc.params = params;
  o->proc.body = body;
  set_type(o, type);
  regist(o);
  return o;
}

object gc_new_macro(object env, int param_count, object params, object body)
{
  return new_proc(MACRO, env, param_count, params, body);
}

object gc_new_func(object env, int param_count, object params, object body)
{
  return new_proc(FUNC, env, param_count, params, body);
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

object gc_new_env(object top, int half_size)
{
  return new_map(ENV, half_size, top);
}

void gc_extend_table(object o)
{
  int i, half_size;
  object *table;
  table = o->map.table;
  half_size = o->map.half_size;
  o->map.entry_count = 0;
  o->map.half_size *= 2;
  o->map.table = gc_alloc(sizeof(object) * o->map.half_size * 2);
  for (i = 0; i < o->map.half_size; i++) o->map.table[i] = NULL;
  for (i = 0; i < half_size; i++)
    if (table[i] != NULL) object_bind(o, table[i], table[i + half_size]);
  gc_free0(sizeof(object) * half_size * 2, table);
}

static void mark_binding(void *key, void *val)
{
  gc_mark(key);
  gc_mark(val);
}

void gc_mark(object o)
{
  int i;
  if (sint_p(o)) return;
  if (alive_p(o)) return;
  set_alive(o);
  switch (object_type(o)) {
    case CONS:
      while (o != object_nil) {
        set_alive(o);    // for stack overflow
        gc_mark(o->cons.car);
        o = o->cons.cdr;
      }
      break;
    case ARRAY:
      for (i = 0; i < o->array.size; i++) gc_mark(o->array.elt[i]);
      break;
    case DICT:
      object_map_foreach(o, mark_binding);
      break;
    case SPECIAL:
    case BUILTINFUNC:
      gc_mark(o->builtin.name);
      break;
    case MACRO:
    case FUNC:
      gc_mark(o->proc.env);
      gc_mark(o->proc.params);
      gc_mark(o->proc.body);
      break;
    case ENV:
      gc_mark(o->map.top);
      object_map_foreach(o, mark_binding);
    default:
      break;
  }
}

static void switch_table(void)
{
  struct xarray *p;
  p = work_table;
  work_table = table;
  table = p;
}

static void sweep_s_expr(void)
{
  int i;
  object o;
  switch_table();
  xarray_reset(table);
  st_reset(&symbol_table);
  st_reset(&keyword_table);
  for (i = 0; i < work_table->size; i++) {
    o = work_table->elt[i];
    if (!alive_p(o)) gc_free(o);
    else {
      set_dead(o);
      switch (object_type(o)) {
        case SYMBOL: st_put(&symbol_table, o); break;
        case KEYWORD: st_put(&keyword_table, o); break;
        default: break;
      }
      regist(o);
    }
  }
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