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
static object free_env;

static struct xarray table;
static struct xarray work_table;
static struct xsplay symbol_table;

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

static object gc_alloc(int size)
{
  if ((gc_used_memory += size) > MAX_HEAP_SIZE) xerror("out of memory.");
  if (gc_used_memory < gc_max_used_memory) gc_max_used_memory = gc_used_memory;
  return xmalloc(size);
}

static void regist(object o)
{ 
  xarray_add(&table, o);
}

object gc_new_env(object top)
{
  object o;
  if (free_env == NULL) {
    o = gc_alloc(sizeof(struct env));
    set_type(o, ENV);
    xsplay_init(&o->env.binding, (int(*)(void *, void *))symcmp);
  } else {
    o = free_env;
    free_env = o->env.top;
  }
  o->env.top = top;
  regist(o);
  return o;
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
  regist(o);
  return o;
}

object gc_new_macro(object env, object params, object body)
{
  return new_lambda(env, params, body, TRUE);
}

object gc_new_lambda(object env, object params, object body)
{
  return new_lambda(env, params, body, FALSE);
}

object gc_new_bytes(int val)
{
  object o;
  o = gc_alloc(sizeof(struct xint));
  set_type(o, XINT);
  o->xint.val = val;
  regist(o);
  return o;
}

object gc_new_xint(int val)
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
  regist(o);
  return o;
}

// TODO free name?
object gc_new_symbol(char *name)
{
  object o;
  if ((o = xsplay_find(&symbol_table, name)) == NULL) {
    o = gc_alloc(sizeof(struct symbol));
    if (name[0] != ':') set_type(o, SYMBOL);
    else set_type(o, KEYWORD);
    o->symbol.name = name;
    xsplay_add(&symbol_table, name, o);
  }
  return o;
}

object gc_new_barray(int size)
{
  object o;
  xassert(size > 0);
  o = gc_alloc(sizeof(struct barray) + size - 1);
  set_type(o, BARRAY);
  o->barray.size = size;
  return o;
}

static void sweep_env(int depth, void *key, void *data)
{
  gc_mark(data);
}

void gc_mark(object o)
{
  if (alivep(o)) return;
  set_alive(o, TRUE);
  switch (type(o)) {
    case ENV:
      gc_mark(o->env.top);
      xsplay_foreach(&o->env.binding, sweep_env);
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
    default: break;
  }
}

static void gc_free(object o)
{
  switch (type(o)) {
    case ENV:
      xsplay_reset(&o->env.binding);
      o->env.top = free_env;
      free_env = o;
      return;    // reuse.
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
  object o;
  for (i = 0; i < table.size; i++) {
    o = table.elt[i];
    if (!alivep(o)) gc_free(o);
    else {
      set_alive(o, FALSE);
      xarray_add(&work_table, o);
    }
  }
  xarray_reset(&table);
  for (i = 0; i < work_table.size; i++) regist(work_table.elt[i]);
  xarray_reset(&work_table);
}

void gc_chance(void)
{
  if (gc_used_memory < GC_CHANCE_MEMORY) return;
  if(GC_LOG_P) {
    printf("before gc(used memory %d[byte])\n", gc_used_memory);
    gc_dump_table();
  }
  ip_mark();
  gc_mark(object_boot);
  sweep_s_expr();
  if (GC_LOG_P) {
    printf("after gc(used memory %d[byte])\n", gc_used_memory);
    gc_dump_table();
  }
}

void gc_init(void)
{
  gc_used_memory = gc_max_used_memory = 0;
  cons_alloc_size = 256;
  free_env = free_cons = NULL;
  xarray_init(&table);
  xarray_init(&work_table);
  xsplay_init(&symbol_table, (int(*)(void *, void *))strcmp);
}

void gc_dump_table(void)
{
  int i;
  char buf[MAX_STR_LEN];
  object o;
  printf("; object table {{{\n");
  printf("; \ttoplevel\t%p\n", object_toplevel);
  printf("; \tnil\t%p\n", object_nil);
  for(i = 0; i < table.size; i++) {
    o = table.elt[i];
    printf("; \t%p\t", o);
    if (typep(o, CONS)) printf("(%p . %p)\n", o->cons.car, o->cons.cdr);
    else if (typep(o, ENV)) printf("-> %p\n", o->env.top);
    else object_describe(o, buf);
  }
  printf("; }}}\n");
}
