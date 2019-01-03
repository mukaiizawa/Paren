// number

#include "std.h"

#include <math.h>

#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

static double double_val(object o)
{
  if (typep(o, XINT)) return (double)o->xint.val;
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
  x = object_bytes[0];
  while (argv != object_nil) {
    FETCH_NUMBER(y);
    if (typep(x, XINT) && typep(y, XINT))
      x = new_xint(x->xint.val + y->xint.val);
    else
      x = new_xfloat(double_val(x) + double_val(y));
  }
  *result = x;
  return TRUE;
}

PRIM(number_multiply)
{
  object x, y;
  x = object_bytes[1];
  while (argv != object_nil) {
    FETCH_NUMBER(y);
    if (typep(x, XINT) && typep(y, XINT))
      x = new_xint(x->xint.val * y->xint.val);
    else
      x = new_xfloat(double_val(x) * double_val(y));
  }
  *result = x;
  return TRUE;
}

PRIM(number_modulo)
{
  object x, y;
  if (argc != 2) return FALSE;
  FETCH_NUMBER(x);
  FETCH_NUMBER(y);
  if (y == 0 || !typep(x, XINT) || !typep(y, XINT)) return FALSE;
  *result = new_xint(x->xint.val % y->xint.val);
  return TRUE;
}

PRIM(number_lt)
{
  object x, y;
  if (argc < 2) return FALSE;
  FETCH_NUMBER(x);
  while (argv != object_nil) {
    FETCH_NUMBER(y);
    if (double_val(x) >= double_val(y)) {
      *result = object_nil;
      return TRUE;
    }
    x = y;
  }
  *result = object_true;
  return TRUE;
}

PRIM(number_ineger_p)
{
  object x;
  if (argc != 1) return FALSE;
  FETCH_NUMBER(x);
  *result = object_bool(typep(x, XINT));
  return TRUE;
}
