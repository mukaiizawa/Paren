// number

#include "std.h"

#include <math.h>

#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static void mark_required_number(void)
{
  ip_mark_exception("required number");
}

static void mark_numeric_over_flow(void)
{
  ip_mark_exception("numeric overflow");
}

static void mark_division_by_zero(void)
{
  ip_mark_exception("division by zero");
}

DEFUN(number_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(number_p(argv->cons.car));
  return TRUE;
}

DEFUN(integer_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(type_p(argv->cons.car, XINT));
  return TRUE;
}

DEFUN(number_equal_p)
{
  double x, y;
  if (!ip_ensure_arguments(argc, 2, FALSE)) return FALSE;
  if (!bi_double(argv->cons.car, &x)) mark_required_number();
  *result = object_true;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!bi_double(argv->cons.car, &y)) mark_required_number();
    if (x != y) {
      *result = object_nil;
      break;
    }
  }
  return TRUE;
}

static int double_add(object argv, object *result)
{
  double x, y, z;
  if (argv == object_nil) return TRUE;
  if (!bi_double(*result, &x) || !bi_double(argv->cons.car, &y)) {
    mark_required_number();
    return FALSE;
  }
  if (!isfinite(z = x + y)) {
    mark_numeric_over_flow();
    return FALSE;
  }
  *result = gc_new_xfloat(z);
  return double_add(argv->cons.cdr, result);
}

static int int64_add(object argv, object *result)
{
  int64_t x, y;
  if (argv == object_nil) return TRUE;
  if (bi_int64(*result, &x) && bi_int64(argv->cons.car, &y)) {
    if ((y > 0 && x > INT64_MAX - y) || (y < 0 && x < INT64_MIN - y)) {
      mark_numeric_over_flow();
      return FALSE;
    }
    *result = gc_new_xint(x + y);
    return int64_add(argv->cons.cdr, result);
  }
  return double_add(argv, result);
}

DEFUN(number_add)
{
  if (!ip_ensure_arguments(argc, 1, FALSE)) return FALSE;
  *result = argv->cons.car;
  return int64_add(argv->cons.cdr, result);
}

static int double_multiply(object argv, object *result)
{
  double x, y, z;
  if (argv == object_nil) return TRUE;
  if (!bi_double(*result, &x) || !bi_double(argv->cons.car, &y)) {
    mark_required_number();
    return FALSE;
  }
  if (!isfinite(z = x * y)) {
    mark_numeric_over_flow();
    return FALSE;
  }
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
  }
  return double_multiply(argv, result);
}

DEFUN(number_multiply)
{
  if (!ip_ensure_arguments(argc, 1, FALSE)) return FALSE;
  *result = argv->cons.car;
  return int64_multiply(argv->cons.cdr, result);
}

static int double_divide(object argv, object *result)
{
  double x, y, z;
  if (argv == object_nil) return TRUE;
  if (!bi_double(*result, &x) || !bi_double(argv->cons.car, &y)) {
    mark_required_number();
    return FALSE;
  }
  if (y == 0) {
    mark_division_by_zero();
    return FALSE;
  }
  if (!isfinite(z = x / y)) {
    mark_numeric_over_flow();
    return FALSE;
  }
  *result = gc_new_xfloat(z);
  return double_divide(argv->cons.cdr, result);
}

static int int64_divide(object argv, object *result)
{
  int64_t x, y;
  if (argv == object_nil) return TRUE;
  if (bi_int64(*result, &x) && bi_int64(argv->cons.car, &y)) {
    if (y == 0) {
      mark_division_by_zero();
      return FALSE;
    } else if (x % y == 0) {
      if(x == INT64_MIN && y == -1) {
        mark_numeric_over_flow();
        return FALSE;
      }
      *result = gc_new_xint(x / y);
      return int64_divide(argv->cons.cdr, result);
    }
  }
  return double_divide(argv, result);
}

DEFUN(number_divide)
{
  if (!ip_ensure_arguments(argc, 1, FALSE)) return FALSE;
  if (argc == 1) {
    *result = object_bytes[1];
    return int64_divide(argv, result);
  }
  *result = argv->cons.car;
  return int64_divide(argv->cons.cdr, result);
}

DEFUN(number_modulo)
{
  int64_t x, y;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!bi_int64(argv->cons.car, &x)) return FALSE;
  if (!bi_int64(argv->cons.cdr->cons.car, &y) || y == 0) return FALSE;
  *result = gc_new_xint(x % y);
  return TRUE;
}

DEFUN(number_lt)
{
  double x, y;
  if (!ip_ensure_arguments(argc, 2, FALSE)) return FALSE;
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

DEFUN(number_floor)
{
  double x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!bi_double(argv->cons.car, &x)) return FALSE;
  *result = gc_new_xfloat(floor(x));
  return TRUE;
}

DEFUN(number_ceiling)
{
  double x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!bi_double(argv->cons.car, &x)) return FALSE;
  *result = gc_new_xfloat(ceil(x));
  return TRUE;
}

DEFUN(number_truncate)
{
  double x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!bi_double(argv->cons.car, &x)) return FALSE;
  *result = gc_new_xfloat(trunc(x));
  return TRUE;
}

DEFUN(number_to_integer)
{
  double x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!bi_double(argv->cons.car, &x)) {
    mark_required_number();
    return FALSE;
  }
  if(!(INT64_MIN <= x && x <= INT64_MAX)) {
    mark_numeric_over_flow();
    return FALSE;
  }
  *result = gc_new_xint((int64_t)x);
  return TRUE;
}

DEFUN(number_to_string)
{
  int64_t i;
  double d;
  struct xbarray x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  xbarray_init(&x);
  if (bi_int64(argv->cons.car, &i)) xbarray_addf(&x, "%d", i);
  else if (bi_double(argv->cons.car, &d)) xbarray_addf(&x, "%g", d);
  else {
    mark_numeric_over_flow();
    return FALSE;
  }
  *result = gc_new_barray_from(STRING, x.elt, x.size);
  xbarray_free(&x);
  return TRUE;
}

DEFUN(bit_and)
{
  int64_t x, y;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!bi_int64(argv->cons.car, &x)) return FALSE;
  if (!bi_int64(argv->cons.cdr->cons.car, &y)) return FALSE;
  if (x < 0 || y < 0) return FALSE;
  *result = gc_new_xint(x & y);
  return TRUE;
}

DEFUN(bit_or)
{
  int64_t x, y;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!bi_int64(argv->cons.car, &x)) return FALSE;
  if (!bi_int64(argv->cons.cdr->cons.car, &y)) return FALSE;
  if (x < 0 || y < 0) return FALSE;
  *result = gc_new_xint(x | y);
  return TRUE;
}

DEFUN(bit_xor)
{
  int64_t x, y;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!bi_int64(argv->cons.car, &x)) return FALSE;
  if (!bi_int64(argv->cons.cdr->cons.car, &y)) return FALSE;
  if (x < 0 || y < 0) return FALSE;
  *result = gc_new_xint(x ^ y);
  return TRUE;
}

static int bits(int64_t x)
{
  int i;
  for (i = 0; i < LINT_BITS; i++)
    if (x < (1LL << i)) return i;
  return LINT_BITS;
}

DEFUN(bit_shift)
{
  int64_t x, y;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!bi_int64(argv->cons.car, &x)) return FALSE;
  if (x < 0) return FALSE;
  if (!bi_int64(argv->cons.cdr->cons.car, &y)) return FALSE;
  if (x != 0) {
    if (y > 0) {
      if ((bits(x) + y) > LINT_BITS) return FALSE;
      x <<= y;
    } else x >>= -y;
  }
  *result = gc_new_xint(x);
  return TRUE;
}
