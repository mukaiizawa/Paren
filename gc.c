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

static long cons_alloc_size;
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
  o->header &= ~ALIVE_MASK;
  if (alivep) o->header |= ALIVE_MASK;
}

static void set_type(object o, int type)
{
  o->header &= ~TYPE_MASK;
  o->header |= type;
}

static char *asstr(int size, char *val)
{
  char *name;
  name = xmalloc(size + 1);
  memcpy(name, val, size);
  name[size] = '\0';
  return name;
}

static object gc_alloc(int size)
{
  if ((gc_used_memory += size) > MAX_HEAP_SIZE) xerror("out of memory.");
  if (gc_used_memory < gc_max_used_memory) gc_max_used_memory = gc_used_memory;
  return xmalloc(size);
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
  xsplay_init(&o->env.binding, (int(*)(void *, void *))symcmp);
  o->env.top = top;
  return regist(o);
}

static object new_lambda(object env, object params, object body, int macro_p)
{
  object o;
  o = gc_alloc(sizeof(struct lambda));
  if (macro_p) set_type(o, MACRO);
  else set_type(o, LAMBDA);
  o->lambda.env = env;
  o->lambda.params = params;
  o->lambda.body = body;
  return regist(o);
}

object gc_new_macro(object env, object params, object body)
{
  return new_lambda(env, params, body, TRUE);
}

object gc_new_lambda(object env, object params, object body)
{
  return new_lambda(env, params, body, FALSE);
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
  int i;
  object o;
  struct cons *p;
  if (free_cons == NULL) {
    free_cons = gc_alloc(sizeof(struct cons) * cons_alloc_size);
    p = (struct cons *)free_cons;
    for (i = 0; i < cons_alloc_size - 1; i++) p[i].cdr = (object)&(p[i + 1]);
    p[cons_alloc_size - 1].cdr = NULL;
    cons_alloc_size *= 2;
  }
  o = free_cons;
  free_cons = o->cons.cdr;
  set_type(o, CONS);
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
      str = asstr(size, val);
      if ((o = xsplay_find(&symbol_table, str)) == NULL) {
        o = new_barray(type, size);
        memcpy(o->barray.elt, val, size);
        xsplay_add(&symbol_table, str, o);
      } else xfree(str);
      return o;
    case KEYWORD:
      str = asstr(size, val);
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

static void mark_binding(int depth, void *key, void *data)
{
  gc_mark(data);
}

void gc_mark(object o)
{
  int i;
  if (alivep(o)) return;
  set_alive(o, TRUE);
  switch (type(o)) {
    case ENV:
      xsplay_foreach(&o->env.binding, mark_binding);
      gc_mark(o->env.top);
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
    case ENV:
      xsplay_free(&o->env.binding);
      break;
    case SYMBOL:
      name = asstr(o->barray.size, o->barray.elt);
      xsplay_delete(&symbol_table, name);
      xfree(name);
      break;
    case KEYWORD:
      name = asstr(o->barray.size, o->barray.elt);
      xsplay_delete(&keyword_table, name);
      xfree(name);
      break;
    case CONS:
      o->cons.cdr = free_cons;
      free_cons = o;
      return;    // reuse.
    default: break;
  }
  gc_used_memory -= object_byte_size(o);
  xfree(o);
}

static void sweep_s_expr(void)
{
  int i;
  char buf[MAX_STR_LEN];
  object o;
  if (GC_LOG_P) printf("***seep\n");
  for (i = 0; i < table.size; i++) {
    o = table.elt[i];
    if (GC_LOG_P) printf("%s", object_describe(o, buf));
    if (alivep(o)) {
      if (GC_LOG_P) printf(",alive");
      set_alive(o, FALSE);
      xarray_add(&work_table, o);
    } else {
      if (GC_LOG_P) printf(",free");
      gc_free(o);
    }
    if (GC_LOG_P) printf("\n");
  }
  xarray_reset(&table);
  for (i = 0; i < work_table.size; i++) regist(work_table.elt[i]);
  xarray_reset(&work_table);
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
  cons_alloc_size = 256;
  free_cons = NULL;
  xarray_init(&table);
  xarray_init(&work_table);
  xsplay_init(&symbol_table, (int(*)(void *, void *))strcmp);
  xsplay_init(&keyword_table, (int(*)(void *, void *))strcmp);
}
