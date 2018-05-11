// garbage collector

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "lex.h"
#include "gc.h"

#define MAX_HEAP_SIZE (256 * 1024 * 1024)
#define GC_CHANCE_MEMORY (512 * 1024)

int gc_used_memory;
int gc_max_used_memory;

static int logp;
static long cons_alloc_size;
static object free_cons;

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
  if (gc_max_used_memory < gc_used_memory) gc_max_used_memory = gc_used_memory;
  return xmalloc(size);
}

static int byte_size(object o)
{
  switch (type(o)) {
    case Env: return sizeof(struct env);
    case Lambda: return sizeof(struct lambda);
    case Cons: return sizeof(struct cons);
    case Fbarray: return sizeof(struct fbarray) + o->fbarray.size;
    case Farray: return sizeof(struct farray)
                 + (o->farray.size * sizeof(object));
    case Xint: return sizeof(struct xint);
    case Xfloat: return sizeof(struct xfloat);
    case Symbol: case Keyword: return sizeof(struct symbol);
    default: xerror("illegal object");
  }
  return -1;
}

static void gc_regist(object o)
{
  xarray_add(&table, o);
}

object gc_new_env(object top)
{
  object o;
  o = gc_alloc(sizeof(struct env));
  set_type(o, Env);
  o->env.top = top;
  xsplay_init(&o->env.binding, (int(*)(void *, void *))symcmp);
  gc_regist(o);
  return o;
}

object gc_new_lambda(object env, object params, object body, int prim_cd)
{
  object o;
  o = gc_alloc(sizeof(struct lambda));
  set_type(o, Lambda);
  o->lambda.env = env;
  o->lambda.params = params;
  o->lambda.body = body;
  o->lambda.prim_cd = prim_cd;
  if (prim_cd < 0) gc_regist(o);
  return o;
}

object gc_new_xint(int val)
{
  object o;
  o = gc_alloc(sizeof(struct xint));
  set_type(o, Xint);
  o->xint.val = val;
  gc_regist(o);
  return o;
}

object gc_new_xfloat(double val)
{
  object o;
  o = gc_alloc(sizeof(struct xfloat));
  set_type(o, Xfloat);
  o->xfloat.val = val;
  gc_regist(o);
  return o;
}

object gc_new_cons(object car, object cdr)
{
  int i;
  object o;
  struct cons *p;
  if (free_cons == NULL) {
    free_cons = gc_alloc(sizeof(struct cons) * cons_alloc_size);
    for (i = 0, p = (struct cons *)free_cons; i < cons_alloc_size - 1; i++)
      p[i].cdr = (object)&(p[i + 1]);
    p[cons_alloc_size - 1].cdr = NULL;
    cons_alloc_size *= 2;
  }
  o = free_cons;
  free_cons = o->cons.cdr;
  set_type(o, Cons);
  o->cons.car = car;
  o->cons.cdr = cdr;
  gc_regist(o);
  return o;
}

object gc_new_barray(int len)
{
  object o;
  o = gc_alloc(sizeof(struct fbarray) + len - 1);
  set_type(o, Fbarray);
  o->fbarray.size = len;
  gc_regist(o);
  return o;
}

object gc_new_fbarray(int len)
{
  object o;
  o = gc_alloc(sizeof(struct farray) + (len - 1) * sizeof(object));
  set_type(o, Farray);
  while (len-- > 0) o->farray.elt[len] = object_nil;
  o->farray.size = len;
  gc_regist(o);
  return o;
}

object gc_new_symbol(char *name)
{
  object o;
  if ((o = xsplay_find(&symbol_table, name)) == NULL) {
    o = gc_alloc(sizeof(struct symbol));
    if (name[0] != ':') set_type(o, Symbol);
    else set_type(o, Keyword);
    o->symbol.name = name;
    xsplay_add(&symbol_table, name, o);
  }
  return o;
}

static void mark_s_expr(object o);

static void sweep_env(int depth, void *key, void *data)
{
  mark_s_expr(data);
}

static void mark_s_expr(object o)
{
  int i;
  if (alivep(o)) return;
  set_alive(o, TRUE);
  switch (type(o)) {
    case Env:
      mark_s_expr(o->env.top);
      xsplay_foreach(&o->env.binding, sweep_env);
      break;
    case Lambda:
      mark_s_expr(o->lambda.env);
      mark_s_expr(o->lambda.params);
      mark_s_expr(o->lambda.body);
      break;
    case Cons:
      mark_s_expr(o->cons.car);
      mark_s_expr(o->cons.cdr);
      break;
    case Farray:
      for (i = 0; i < o->farray.size; i++) mark_s_expr(o->farray.elt[i]);
      break;
    default: break;
  }
}

static void gc_free(object o)
{
  gc_used_memory -= byte_size(o);
  switch (type(o)) {
    case Env:
      xsplay_free(&o->env.binding);
      break;
    case Cons:
      o->cons.cdr = free_cons;
      free_cons = o;
      return;
    case Fbarray:
      xfree(o->fbarray.elt);
      break;
    default: break;
  }
  xfree(o);
}

static void sweep_s_expr()
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
  for (i = 0; i < work_table.size; i++) xarray_add(&table, work_table.elt[i]);
  xarray_reset(&work_table);
}

void gc_chance()
{
  if (gc_used_memory < GC_CHANCE_MEMORY) return;
  if (logp) {
    printf("before gc ");
    gc_dump_table();
  }
  mark_s_expr(object_boot_lambda);
  sweep_s_expr();
  if (logp) {
    printf("after gc ");
    gc_dump_table();
  }
}

void gc_init(int gc_logp)
{
  logp = gc_logp;
  gc_used_memory = gc_max_used_memory = 0;
  cons_alloc_size = 256;
  free_cons = NULL;
  xarray_init(&table);
  xarray_init(&work_table);
  xsplay_init(&symbol_table, (int(*)(void *, void *))strcmp);
}

void gc_dump_table(void)
{
  int i;
  object o;
  printf("; free_cons: %p\n", free_cons);
  printf("; object table {{{\n");
  for(i = 0; i < table.size; i++) {
    o = table.elt[i];
    printf("; \t%p\t", o);
    if (typep(o, Cons)) printf("(%p . %p)\n", o->cons.car, o->cons.cdr);
    else object_dump(o);
  }
  printf("; }}}\n");
}
