// garbage collector

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "lex.h"
#include "gc.h"

object toplevel;

static int count;
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

static void gc_regist(object o)
{
  xarray_add(&table, o);
  if (count++ == 20) {
    count = 0;
    gc_full();
  }
}

object gc_new_lambda(object top, object params, object body, int prim_cd)
{
  object o;
  o = xmalloc(sizeof(struct lambda));
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
  o = xmalloc(sizeof(struct xint));
  o->header.type = xint;
  o->xint.val = val;
  gc_regist(o);
  return o;
}

object gc_new_xfloat(double val)
{
  object o;
  o = xmalloc(sizeof(struct xfloat));
  o->header.type = xfloat;
  o->xfloat.val = val;
  gc_regist(o);
  return o;
}

object gc_new_cons(object car, object cdr)
{
  object o;
  o = xmalloc(sizeof(struct cons));
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
    o = xmalloc(sizeof(struct symbol));
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
  o = xmalloc(sizeof(struct fbarray) + len - 1);
  o->header.type = fbarray;
  gc_regist(o);
  return o;
}

object gc_new_fbarray(int len)
{
  object o;
  o = xmalloc(sizeof(struct farray) + (len - 1) * sizeof(object));
  o->header.type = farray;
  while (len-- > 0) o->farray.elt[len] = object_nil;
  gc_regist(o);
  return o;
}

static void mark_s_expr(object o);

static void lambda_sweep(int depth, void *key, void *data)
{
  mark_s_expr(data);
}

static void mark_lambda(object o)
{
  object p;
  if (o->lambda.prim_cd < 0)
    xsplay_foreach(&o->lambda.binding, lambda_sweep);
  for (p = o->lambda.body; p != object_nil; p = p->cons.cdr)
    mark_s_expr(p);
}

static void mark_cons(object o)
{
  object p;
  for (p = o; p != object_nil; p = p->cons.cdr) {
    mark_s_expr(p->cons.car);
    mark_s_expr(p->cons.cdr);
  }
}

static void mark_farray(object o)
{
  int i;
  for (i = 0; i < o->farray.size; i++) mark_s_expr(o->farray.elt[i]);
}

static void mark_s_expr(object o)
{
  if (o->header.alivep) return;
  o->header.alivep = TRUE;
  switch (o->header.type) {
    case lambda: mark_lambda(o); break;
    case cons: mark_cons(o); break;
    case farray: mark_farray(o); break;
    default: break;
  }
}

static void free_s_expr(object o)
{
  printf("free >>>>>>>>>>>>>>>>>>>>>>");
  object_dump(o);
  // switch (o->header.type) {
  //   case cons: xfree(o);
  //   default: return;
  // }
  printf("free <<<<<<<<<<<<<<<<<<<<<<");
}

void gc_full(void)
{
  int i;
  object o;
  xarray_reset(&work_table);
  for (i = 0; i < table.size; i++) ((object)table.elt[i])->header.alivep = 0;
  mark_s_expr(toplevel);
  for (i = 0; i < table.size; i++) {
    o = table.elt[i];
    if (o->header.alivep) xarray_add(&work_table, o);
    else free_s_expr(o);
  }
  xarray_reset(&table);
  for (i = 0; i < work_table.size; i++) xarray_add(&table, work_table.elt[i]);
  gc_dump_table();
}

void gc_init(void)
{
  count = 0;
  xarray_init(&work_table);
  xarray_init(&table);
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
  printf("object table <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<n");
}
