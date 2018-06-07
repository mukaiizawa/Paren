// list

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

PRIM(cons)
{
  if (argc != 2) return FALSE;
  *result = gc_new_cons(argv->cons.car, argv->cons.cdr->cons.car);
  return TRUE;
}

PRIM(car)
{
  object o;
  o = argv->cons.car;
  if (argc != 1 || !listp(o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.car;
  return TRUE;
}

PRIM(xcar)
{
  object o, p;
  o = argv->cons.car;
  if (argc != 2 || !typep(o, Cons)) return FALSE;
  p = argv->cons.cdr->cons.car;
  o->cons.car = p;
  *result = p;
  return TRUE;
}

PRIM(cdr)
{
  object o;
  o = argv->cons.car;
  if (argc != 1 || !listp(o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.cdr;
  return TRUE;
}

PRIM(xcdr)
{
  object o, p;
  o = argv->cons.car;
  if (argc != 2 || !typep(o, Cons)) return FALSE;
  p = argv->cons.cdr->cons.car;
  o->cons.cdr = p;
  *result = p;
  return TRUE;
}
