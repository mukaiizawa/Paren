// list

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

PRIM(cons)
{
  if (argc != 2 || !listp(argv->cons.cdr->cons.car)) return FALSE;
  *result = gc_new_cons(argv->cons.car, argv->cons.cdr->cons.car);
  return TRUE;
}

PRIM(car)
{
  object o, p;
  o = object_nth(argv, 0);
  if ((argc != 1 && argc != 2) || !listp(o)) return FALSE;
  if (argc == 1) {
    if (o == object_nil) *result = object_nil;
    else *result = o->cons.car;
  } else {
    if (o == object_nil) return FALSE;
    p = object_nth(argv, 1);
    o->cons.car = p;
    *result = p;
  }
  return TRUE;
}

PRIM(cdr)
{
  object o, p;
  o = object_nth(argv, 0);
  if ((argc != 1 && argc != 2) || !listp(o)) return FALSE;
  if (argc == 1) {
    if (o == object_nil) *result = object_nil;
    else *result = o->cons.cdr;
  } else {
    if (o == object_nil) return FALSE;
    p = object_nth(argv, 1);
    o->cons.cdr = p;
    *result = p;
  }
  return TRUE;
}