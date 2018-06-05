// list

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

PRIM(samep)
{
  object o, p;
  if (argc != 2) return FALSE;
  o = object_nth(argv, 0);
  p = object_nth(argv, 1);
  *result = object_bool(o == p);
  return TRUE;
}

PRIM(atomp)
{
  if (argc != 1) return FALSE;
  *result = object_bool(!typep(argv->cons.car, Cons));
  return TRUE;
}

PRIM(cons)
{
  object o, p;
  if (argc != 2) return FALSE;
  o = object_nth(argv, 0);
  p = object_nth(argv, 1);
  *result = gc_new_cons(o, p);
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
