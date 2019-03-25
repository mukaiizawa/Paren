// cons

#include "std.h"
#include "xsplay.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

PRIM(cons_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, CONS));
  return TRUE;
}

PRIM(cons)
{
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!listp(argv->cons.cdr->cons.car)) {
    ip_mark_error("part of cdr must be list");
    return FALSE;
  }
  *result = gc_new_cons(argv->cons.car, argv->cons.cdr->cons.car);
  return TRUE;
}

PRIM(car)
{
  object o, p;
  if (!ip_ensure_arguments(argc, 1, 2)) return FALSE;
  if (!listp(o = argv->cons.car)) return FALSE;
  if (argc == 1) {
    if (o == object_nil) *result = object_nil;
    else *result = o->cons.car;
  } else {
    if (o == object_nil) return FALSE;
    p = argv->cons.cdr->cons.car;
    o->cons.car = p;
    *result = p;
  }
  return TRUE;
}

PRIM(cdr)
{
  object o, p;
  if (!ip_ensure_arguments(argc, 1, 2)) return FALSE;
  if (!listp(o = argv->cons.car)) return FALSE;
  if (argc == 1) {
    if (o == object_nil) *result = object_nil;
    else *result = o->cons.cdr;
  } else {
    if (o == object_nil) return FALSE;
    p = argv->cons.cdr->cons.car;
    o->cons.cdr = p;
    *result = p;
  }
  return TRUE;
}
