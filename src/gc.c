// garbage collector

#include <string.h>

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "lex.h"

static struct xarray table;
static struct xsplay symbol_table;

static int symcmp(object o, object p)
{
  xassert(o->header.type == symbol && o->header.type == p->header.type);
  return o - p;
}

void gc_regist(object o)
{
  xarray_add(&table, o);
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
  gc_regist(o);
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
    gc_regist(o);
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

void gc_full(void)
{
}

void gc_init(void)
{
  xsplay_init(&symbol_table, (int(*)(void *, void *))strcmp);
  xarray_init(&table);
}

void gc_dump_table(void)
{
  int i;
  printf("* object table >\n");
  for(i = 0; i < table.size; i++) {
    printf("%p\t", table.elt[i]);
    object_dump(table.elt[i]);
  }
  printf("<\n");
}
