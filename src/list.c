// list

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "prim.h"

PRIM(samep)
{
  int argc;
  object o, p;
  ARGC(argc);
  if (argc != 2) return FALSE;
  ARG(0, o);
  ARG(1, p);
  *result = object_bool(o == p);
  return TRUE;
}

PRIM(cons)
{
  int argc;
  object o, p;
  ARGC(argc);
  if (argc != 2) return FALSE;
  ARG(0, o);
  ARG(1, p);
  *result = gc_new_cons(o, p);
  return TRUE;
}

PRIM(car)
{
  int argc;
  object o, p;
  ARGC(argc);
  ARG(0, o);
  if ((argc != 1 && argc != 2) || !listp(o)) return FALSE;
  if (argc == 1) {
    if (o == object_nil) *result = object_nil;
    else *result = o->cons.car;
  } else {
    if (o == object_nil) return FALSE;
    ARG(1, p);
    o->cons.car = p;
    *result = p;
  }
  return TRUE;
}

PRIM(cdr)
{
  int argc;
  object o, p;
  ARGC(argc);
  ARG(0, o);
  if ((argc != 1 && argc != 2) || !listp(o)) return FALSE;
  if (argc == 1) {
    if (o == object_nil) *result = object_nil;
    else *result = o->cons.cdr;
  } else {
    if (o == object_nil) return FALSE;
    ARG(1, p);
    o->cons.cdr = p;
    *result = p;
  }
  return TRUE;
}
