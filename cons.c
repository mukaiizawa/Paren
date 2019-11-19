// cons

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int ensure_cons(object o, object *result)
{
  if (!typep(o, CONS)) {
    ip_mark_error("requires cons");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

static int ensure_list(object o, object *result)
{
  if (!listp(o)) {
    ip_mark_error("requires list");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

static int ensure_cdr(object o, object *result)
{
  if (!listp(o)) {
    ip_mark_error("part of cdr must be list");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

PRIM(cons_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, CONS));
  return TRUE;
}

PRIM(cons)
{
  object o;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!ensure_cdr(argv->cons.cdr->cons.car, &o)) return FALSE;
  *result = gc_new_cons(argv->cons.car, o);
  return TRUE;
}

PRIM(car)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_list(argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.car;
  return TRUE;
}

PRIM(set_car)
{
  object o, p;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!ensure_cons(argv->cons.car, &o)) return FALSE;
  p = argv->cons.cdr->cons.car;
  o->cons.car = p;
  *result = p;
  return TRUE;
}

PRIM(cdr)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_list(argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.cdr;
  return TRUE;
}

PRIM(set_cdr)
{
  object o, p;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!ensure_cons(argv->cons.car, &o)) return FALSE;
  if (!ensure_cdr(argv->cons.cdr->cons.car, &p)) return FALSE;
  p = argv->cons.cdr->cons.car;
  o->cons.cdr = p;
  *result = p;
  return TRUE;
}
