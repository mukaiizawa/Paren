// number

#include "std.h"

#include <math.h>

#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

static int double_add(object argv, object *result)
{
  double x, y, z;
  if (argv == object_nil) return TRUE;
  if (!bi_double(*result, &x)) return FALSE;
  if (!bi_double(argv->cons.car, &y)) return FALSE;
  if (!isfinite(z = x + y)) return FALSE;
  *result = gc_new_xfloat(z);
  return double_add(argv->cons.cdr, result);
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
  } else return double_add(argv, result);
}

PRIM(number_add)
{
  *result = object_bytes[0];
  return int64_add(argv, result);
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
