// garbage collector

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "lex.h"

void gc_regist(object o)
{
  xarray_add(&object_table, o);
}

object gc_new_cons(object car, object cdr)
{
  object o;
  o = object_alloc();
  o->header.type = cons;
  o->cons.car = car;
  o->cons.cdr = cdr;
  gc_regist(o);
  return o;
}

void gc_full(void)
{
}

void gc_init(void)
{
}
