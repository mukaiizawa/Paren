// number

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

#define PARSE_ARGS(x, y) { \
  if (argc != 2) return FALSE; \
  if (!numberp(x = argv->cons.car)) return FALSE; \
  if (!numberp(y = argv->cons.cdr->cons.car)) return FALSE; \
}

static int numberp(object o)
{
  switch (type(o)) {
    case Xint:
    case Xfloat:
      return TRUE;
    default: 
      return FALSE;
  }
}

static double double_val(object o)
{
  if (typep(o, Xint)) return (double)o->xint.val;
  return o->xfloat.val;
}

static object new_xfloat(double val)
{
  return gc_new_xfloat(val);
}

static object new_xint(int64_t val)
{
  if (XINT_MIN <= val && val <= XINT_MAX) return gc_new_xint(val);
  return new_xfloat((double)val);
}

PRIM(number_add)
{
  object x, y;
  PARSE_ARGS(x, y);
  if (typep(x, Xint) && typep(y, Xint))
    *result = new_xint(x->xint.val + y->xint.val);
  else
    *result = new_xfloat(double_val(x) + double_val(y));
  return TRUE;
}

PRIM(number_multiply)
{
  object x, y;
  PARSE_ARGS(x, y);
  if (typep(x, Xint) && typep(y, Xint))
    *result = new_xint(x->xint.val * y->xint.val);
  else
    *result = new_xfloat(double_val(x) * double_val(y));
  return TRUE;
}

PRIM(number_eq)
{
  object x, y;
  PARSE_ARGS(x, y);
  *result = object_bool(double_val(x) == double_val(y));
  return TRUE;
}

PRIM(number_lt)
{
  object x, y;
  PARSE_ARGS(x, y);
  *result = object_bool(double_val(x) < double_val(y));
  return TRUE;
}
