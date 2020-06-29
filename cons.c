// cons

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "splay.h"
#include "object.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

DEFUN(cons_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, CONS));
  return TRUE;
}

DEFUN(cons)
{
  object o;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_list(argv->cons.cdr->cons.car, &o)) return FALSE;
  *result = gc_new_cons(argv->cons.car, o);
  return TRUE;
}

DEFUN(list)
{
  *result = argv;
  return TRUE;
}

DEFUN(car)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.car;
  return TRUE;
}

DEFUN(set_car)
{
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_type(argv->cons.car, CONS, &o)) return FALSE;
  p = argv->cons.cdr->cons.car;
  o->cons.car = p;
  *result = p;
  return TRUE;
}

DEFUN(cdr)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.cdr;
  return TRUE;
}

DEFUN(set_cdr)
{
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_type(argv->cons.car, CONS, &o)) return FALSE;
  if (!bi_arg_list(argv->cons.cdr->cons.car, &p)) return FALSE;
  o->cons.cdr = p;
  *result = p;
  return TRUE;
}

DEFUN(last_cons)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  if (o != object_nil) {
    while (o->cons.cdr != object_nil) o = o->cons.cdr;
  }
  *result = o;
  return TRUE;
}

DEFUN(xreverse)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_reverse(o);
  else *result = object_reverse(o);
  return TRUE;
}

DEFUN(assoc)
{
  object o, k;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  k = argv->cons.cdr->cons.car;
  while (o != object_nil) {
    if (o->cons.car == k) {
      if (!bi_arg_type(o->cons.cdr, CONS, &o)) break;
      *result = o->cons.car;
      return TRUE;
    }
    if (!bi_arg_type(o->cons.cdr, CONS, &o)) break;
    o = o->cons.cdr;
  }
  ip_mark_error("property not found");
  return FALSE;
}

DEFUN(set_assoc)
{
  object o, k, v;
  if (!bi_argc_range(argc, 3, 3)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  k = argv->cons.cdr->cons.car;
  v = argv->cons.cdr->cons.cdr->cons.car;
  while (o != object_nil) {
    if (o->cons.car == k) {
      if (!bi_arg_type(o->cons.cdr, CONS, &o)) break;
      o->cons.car = v;
      *result = v;
      return TRUE;
    }
    if (!bi_arg_type(o->cons.cdr, CONS, &o)) break;
    o = o->cons.cdr;
  }
  ip_mark_error("property not found");
  return FALSE;
}
