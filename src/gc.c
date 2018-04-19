// garbage collector

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "lex.h"
#include "gc.h"

int gc_used_memory;
int gc_max_used_memory;

static struct xarray table;
static struct xarray work_table;
static struct xsplay symbol_table;

static int symcmp(object o, object p)
{
  intptr_t i;
  xassert(o->header.type == symbol && p->header.type == symbol);
  if ((i = (intptr_t)o - (intptr_t)p) == 0) return 0;
  if (i > 0) return 1;
  return -1;
}

static object gc_alloc(int size)
{
  if (gc_used_memory += size > GC_MAX_MEMORY) xerror("out of memory.");
  if (gc_max_used_memory < gc_used_memory)
    gc_max_used_memory = gc_used_memory;
  return xmalloc(size);
}

static void gc_regist(object o)
{
  xarray_add(&table, o);
}

object gc_new_lambda(object top, object params, object body, int prim_cd)
{
  object o;
  o = gc_alloc(sizeof(struct lambda));
  o->header.type = lambda;
  o->lambda.top = top;
  o->lambda.params = params;
  o->lambda.body = body;
  o->lambda.prim_cd = prim_cd;
  xsplay_init(&o->lambda.binding, (int(*)(void *, void *))symcmp);
  if (prim_cd < 0) gc_regist(o);
  return o;
}

object gc_new_xint(int val)
{
  object o;
  o = gc_alloc(sizeof(struct xint));
  o->header.type = xint;
  o->xint.val = val;
  gc_regist(o);
  return o;
}

object gc_new_xfloat(double val)
{
  object o;
  o = gc_alloc(sizeof(struct xfloat));
  o->header.type = xfloat;
  o->xfloat.val = val;
  gc_regist(o);
  return o;
}

object gc_new_cons(object car, object cdr)
{
  object o;
  o = gc_alloc(sizeof(struct cons));
  o->header.type = cons;
  o->cons.car = car;
  o->cons.cdr = cdr;
  gc_regist(o);
  return o;
}

object gc_new_symbol(char *name)
{
  object o;
  if ((o = xsplay_find(&symbol_table, name)) == NULL) {
    o = gc_alloc(sizeof(struct symbol));
    if (name[0] != ':') o->header.type = symbol;
    else o->header.type = keyword;
    o->symbol.name = name;
    xsplay_add(&symbol_table, name, o);
  }
  return o;
}

object gc_new_barray(int len)
{
  object o;
  o = gc_alloc(sizeof(struct fbarray) + len - 1);
  o->header.type = fbarray;
  gc_regist(o);
  return o;
}

object gc_new_fbarray(int len)
{
  object o;
  o = gc_alloc(sizeof(struct farray) + (len - 1) * sizeof(object));
  o->header.type = farray;
  while (len-- > 0) o->farray.elt[len] = object_nil;
  gc_regist(o);
  return o;
}

static void mark_s_expr(object o);

static void mark_sweep(int depth, void *key, void *data)
{
  mark_s_expr(data);
}

static void mark_s_expr(object o)
{
  int i;
  if (o->header.alivep) return;
  o->header.alivep = TRUE;
  switch (o->header.type) {
    case lambda:
      mark_s_expr(o->lambda.top);
      mark_s_expr(o->lambda.params);
      mark_s_expr(o->lambda.body);
      if (o->lambda.prim_cd < 0)
        xsplay_foreach(&o->lambda.binding, mark_sweep);
      break;
    case cons:
      mark_s_expr(o->cons.car);
      mark_s_expr(o->cons.cdr);
      break;
    case farray:
      for (i = 0; i < o->farray.size; i++) mark_s_expr(o->farray.elt[i]);
      break;
    default: break;
  }
}

static void free_s_expr(object o)
{
  gc_used_memory -= sizeof(o);
  xfree(o);
}

void gc_chance(void)
{
  if (gc_used_memory > GC_CHANCE_MEMORY) gc_full();
}

void gc_full(void)
{
  int i;
  object o;
  xarray_reset(&work_table);
  for (i = 0; i < table.size; i++) {
    o = table.elt[i];
    o->header.alivep = FALSE;
  }
  mark_s_expr(toplevel);
  for (i = 0; i < table.size; i++) {
    o = table.elt[i];
    if (o->header.alivep) xarray_add(&work_table, o);
    else free_s_expr(o);
  }
  xarray_reset(&table);
  for (i = 0; i < work_table.size; i++) xarray_add(&table, work_table.elt[i]);
}

void gc_init(void)
{
  gc_used_memory = gc_max_used_memory = 0;
  xarray_init(&table);
  xarray_init(&work_table);
  xsplay_init(&symbol_table, (int(*)(void *, void *))strcmp);
}

void gc_dump_table(void)
{
  int i;
  printf("object table >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
  for(i = 0; i < table.size; i++) {
    printf("%p\t", table.elt[i]);
    object_dump(table.elt[i]);
  }
  printf("object table <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<");
}
