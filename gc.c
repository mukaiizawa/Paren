// garbage collector

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "lex.h"
#include "ip.h"
#include "gc.h"

int gc_used_memory;
int gc_max_used_memory;

static object free_cons;

static struct xarray table;
static struct xarray work_table;
static struct xsplay symbol_table;
static struct xsplay keyword_table;

static int alivep(object o)
{
  return o->header & ALIVE_MASK;
}

static void set_alive(object o, int alivep)
{
  if (alivep) o->header |= ALIVE_MASK;
  else o->header &= ~ALIVE_MASK;
}

static void set_type(object o, int type)
{
  o->header &= ~TYPE_MASK;
  o->header |= type;
}

static char *symbol_name(int size, char *val)
{
  char *name;
  name = xmalloc(size + 1);
  memcpy(name, val, size);
  name[size] = '\0';
  return name;
}

static object gc_alloc(int size)
{
  object o;
  if ((gc_used_memory += size) > MAX_HEAP_SIZE) xerror("out of memory.");
  if (gc_used_memory < gc_max_used_memory) gc_max_used_memory = gc_used_memory;
  o = xmalloc(size);
  set_alive(o, FALSE);
  return o;
}

static object regist(object o)
{ 
  xarray_add(&table, o);
  return o;
}

object gc_new_env(object top)
{
  object o;
  o = gc_alloc(sizeof(struct env));
  set_type(o, ENV);
  o->env.top = top;
  o->env.binding = object_nil;
  return regist(o);
}

static object new_lambda(object env, object params, object body)
{
  object o;
  xassert(typep(env, ENV));
  o = gc_alloc(sizeof(struct lambda));
  o->lambda.env = env;
  o->lambda.params = params;
  o->lambda.body = body;
  return regist(o);
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

object gc_new_bytes(int64_t val)
{
  object o;
  o = gc_alloc(sizeof(struct xint));
  set_type(o, XINT);
  o->xint.val = val;
  return regist(o);
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
  return regist(o);
}

object gc_new_cons(object car, object cdr)
{
  object o;
  if (free_cons == NULL) {
    o = gc_alloc(sizeof(struct cons));
    set_type(o, CONS);
  } else {
    o = free_cons;
    free_cons = o->cons.cdr;
  }
  o->cons.car = car;
  o->cons.cdr = cdr;
  return regist(o);
}

static object new_barray(int type, int size)
{
  object o;
  xassert(size >= 0);
  o = gc_alloc(sizeof(struct barray) + size - 1);
  set_type(o, type);
  memset(o->barray.elt, 0, size);
  o->barray.size = size;
  return regist(o);
}

object gc_new_barray(int type, int size)
{
  return new_barray(type, size);
}

object gc_new_barray_from(int type, int size, char *val)
{
  object o;
  char *str;
  switch (type) {
    case SYMBOL:
      str = symbol_name(size, val);
      if ((o = xsplay_find(&symbol_table, str)) == NULL) {
        o = new_barray(type, size);
        memcpy(o->barray.elt, val, size);
        xsplay_add(&symbol_table, str, o);
      } else xfree(str);
      return o;
    case KEYWORD:
      str = symbol_name(size, val);
      if ((o = xsplay_find(&keyword_table, str)) == NULL) {
        o = new_barray(type, size);
        memcpy(o->barray.elt, val, size);
        xsplay_add(&keyword_table, str, o);
      } else xfree(str);
      return o;
    case STRING:
    case BARRAY:
      o = new_barray(type, size);
      memcpy(o->barray.elt, val, size);
      return o;
    default:
      xassert(FALSE);
      return NULL;
  }
}

object gc_new_array(int size)
{
  int i;
  object o;
  xassert(size >= 0);
  o = gc_alloc(sizeof(struct array) + sizeof(object) * (size - 1));
  set_type(o, ARRAY);
  for (i = 0; i < size; i++) o->array.elt[i] = object_nil;
  o->array.size = size;
  return regist(o);
}

void gc_mark(object o)
{
  int i;
  if (alivep(o)) return;
  set_alive(o, TRUE);
  switch (type(o)) {
    case ENV:
      gc_mark(o->env.top);
      gc_mark(o->env.binding);
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
    default: break;
  }
}

static void gc_free(object o)
{
  char *name;
  switch (type(o)) {
    case CONS:
      o->cons.cdr = free_cons;
      free_cons = o;
      return;
    case SYMBOL:
      name = symbol_name(o->barray.size, o->barray.elt);
      xsplay_delete(&symbol_table, name);
      xfree(name);
      break;
    case KEYWORD:
      name = symbol_name(o->barray.size, o->barray.elt);
      xsplay_delete(&keyword_table, name);
      xfree(name);
      break;
    default: break;
  }
  gc_used_memory -= object_byte_size(o);
  xfree(o);
}

static void sweep_s_expr(void)
{
  int i;
  object o;
  xarray_reset(&work_table);
  for (i = 0; i < table.size; i++) {
    o = table.elt[i];
    if (alivep(o)) {
      set_alive(o, FALSE);
      xarray_add(&work_table, o);
    } else gc_free(o);
  }
  xarray_reset(&table);
  for (i = 0; i < work_table.size; i++) regist(work_table.elt[i]);
}

void gc_chance(void)
{
  if (gc_used_memory < GC_CHANCE_MEMORY) return;
  if (GC_LOG_P) printf("before gc(used memory %d[byte])\n", gc_used_memory);
  ip_mark();
  sweep_s_expr();
  if (GC_LOG_P) printf("after gc(used memory %d[byte])\n", gc_used_memory);
}

void gc_init(void)
{
  gc_used_memory = gc_max_used_memory = 0;
  free_cons = NULL;
  xarray_init(&table);
  xarray_init(&work_table);
  xsplay_init(&symbol_table, (int(*)(void *, void *))strcmp);
  xsplay_init(&keyword_table, (int(*)(void *, void *))strcmp);
}
