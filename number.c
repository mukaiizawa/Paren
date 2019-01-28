// number

#include "std.h"

#include <math.h>

#include "xbarray.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

PRIM(number_to_integer)
{
  double x;
  if (argc != 1) return FALSE;
  switch (type(argv->cons.car)) {
    case XINT:
      *result = argv->cons.car;
      break;
    case XFLOAT:
      if (!bi_double(argv->cons.car, &x)) return FALSE;
      *result = gc_new_xint((int64_t)x);
      break;
    default:
      return FALSE;
  }
  return TRUE;
}

static int to_string(object o, object *result)
{
  int64_t i;
  double d;
  struct xbarray x;
  xbarray_init(&x);
  switch (type(o)) {
    case XINT:
      if (!bi_int64(o, &i)) return FALSE;
      xbarray_addf(&x, "%d", i);
      break;
    case XFLOAT:
      if (!bi_double(o, &d)) return FALSE;
      xbarray_addf(&x, "%g", d);
      break;
    default:
      return FALSE;
  }
  *result = gc_new_barray_from(STRING, x.size, x.elt);
  xbarray_free(&x);
  return TRUE;
}

PRIM(number_to_string)
{
  if (argc != 1) return FALSE;
  return to_string(argv->cons.car, result);
}

static int barray_add(object argv, object *result)
{
  object x, y;
  if (argv == object_nil) return TRUE;
  if (typep(*result, STRING)) x = *result;
  else if (!to_string(*result, &x)) return FALSE;
  if (typep(argv->cons.car, STRING)) y = argv->cons.car;
  else if (!to_string(argv->cons.car, &y)) return FALSE;
  *result = gc_new_barray(STRING, x->barray.size + y->barray.size);
  memcpy((*result)->barray.elt, x->barray.elt, x->barray.size);
  memcpy((*result)->barray.elt + x->barray.size, y->barray.elt, y->barray.size);
  return barray_add(argv->cons.cdr, result);
}

static int double_add(object argv, object *result)
{
  double x, y, z;
  if (argv == object_nil) return TRUE;
  if (bi_double(*result, &x) && bi_double(argv->cons.car, &y)) {
    if (!isfinite(z = x + y)) return FALSE;
    *result = gc_new_xfloat(z);
    return double_add(argv->cons.cdr, result);
  }
  return barray_add(argv, result);
}

static int int64_add(object argv, object *result)
{
  int64_t x, y;
  if (argv == object_nil) return TRUE;
  if (bi_int64(*result, &x) && bi_int64(argv->cons.car, &y)) {
    if (y > 0 && x > INT64_MAX - y) return FALSE;
    if (y < 0 && x < INT64_MIN - y) return FALSE;
    *result = gc_new_xint(x + y);
    return int64_add(argv->cons.cdr, result);
  }
  return double_add(argv, result);
}

PRIM(number_add)
{
  if (argv == 0) return FALSE;
  *result = argv->cons.car;
  if (numberp(*result)) return int64_add(argv->cons.cdr, result);
  return barray_add(argv->cons.cdr, result);
}

static int double_multiply(object argv, object *result)
{
  double x, y, z;
  if (argv == object_nil) return TRUE;
  if (!bi_double(*result, &x)) return FALSE;
  if (!bi_double(argv->cons.car, &y)) return FALSE;
  if (!isfinite(z = x * y)) return FALSE;
  *result = gc_new_xfloat(z);
  return double_multiply(argv->cons.cdr, result);
}

static int int64_multiply(object argv, object *result)
{
  int64_t x, y;
  if (argv == object_nil) return TRUE;
  if (bi_int64(*result, &x) && bi_int64(argv->cons.car, &y)) {
    if (x > 0) {
      if (y > 0) {
        if (x > INT64_MAX / y) return FALSE;
      } else {
        if (y < INT64_MIN / x) return FALSE;
      }
    } else {
      if (y > 0) {
        if (x < INT64_MIN / y) return FALSE;
      } else {
        if (x != 0 && y < INT64_MAX / x) return FALSE;
      }
    }
    *result = gc_new_xint(x * y);
    return int64_multiply(argv->cons.cdr, result);
  } else return double_multiply(argv, result);
}

PRIM(number_multiply)
{
  *result = object_bytes[1];
  return int64_multiply(argv, result);
}

PRIM(number_modulo)
{
  int64_t x, y;
  if (argc != 2) return FALSE;
  if (!bi_int64(argv->cons.car, &x)) return FALSE;
  if (!bi_int64(argv->cons.cdr->cons.car, &y) || y == 0) return FALSE;
  *result = gc_new_xint(x % y);
  return TRUE;
}

PRIM(number_lt)
{
  double x, y;
  if (argc < 2) return FALSE;
  if (!bi_double(argv->cons.car, &x)) return FALSE;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!bi_double(argv->cons.car, &y)) return FALSE;
    if (x >= y) {
      *result = object_nil;
      return TRUE;
    }
    x = y;
  }
  *result = object_true;
  return TRUE;
}
