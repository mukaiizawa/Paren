// number.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int mark_required_integer(void)
{
  return ip_mark_error("expected integer");
}

static int mark_required_positive_int(void)
{
  return ip_mark_error("expected positive integer");
}

static int mark_required_number(void)
{
  return ip_mark_error("expected number");
}

static int mark_numeric_over_flow(void)
{
  return ip_mark_error("numeric overflow");
}

static int mark_division_by_zero(void)
{
  return ip_mark_error("division by zero");
}

DEFUN(number_3f_)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  switch (object_type(argv->cons.car)) {
    case SINT:
    case XINT:
    case XFLOAT:
      *result = object_true;
      return TRUE;
    default:
      *result = object_nil;
      return TRUE;
  }
}

DEFUN(int_3f_)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  switch (object_type(argv->cons.car)) {
    case SINT:
    case XINT:
      *result = object_true;
      return TRUE;
    default:
      *result = object_nil;
      return TRUE;
  }
  return TRUE;
}

static int double_add(double x, object argv, object *result)
{
  int64_t i;
  double d;
  if (!isfinite(x)) return mark_numeric_over_flow();
  if (argv == object_nil) {
    *result = gc_new_xfloat(x);
    return TRUE;
  }
  if (bi_int64(argv->cons.car, &i))
    return double_add(x + (double)i, argv->cons.cdr, result);
  if (bi_double(argv->cons.car, &d))
    return double_add(x + d, argv->cons.cdr, result);
  return mark_required_number();
}

static int int64_add(int64_t x, object argv, object *result)
{
  int64_t y;
  if (argv == object_nil) {
    *result = gc_new_xint(x);
    return TRUE;
  }
  if (bi_int64(argv->cons.car, &y)) {
    if (y > 0 && x > INT64_MAX - y) return mark_numeric_over_flow();
    if (y < 0 && x < INT64_MIN - y) return mark_numeric_over_flow();
    return int64_add(x + y, argv->cons.cdr, result);
  }
  return double_add((double)x, argv, result);
}

DEFUN(_2b_)
{
  if (argc == 0) {
    *result = gc_new_xint(0);
    return TRUE;
  }
  return int64_add(0, argv, result);
}

static int double_multiply(double dx, object argv, object *result)
{
  int64_t iy;
  double dy;
  if (!isfinite(dx)) return mark_numeric_over_flow();
  if (argv == object_nil) {
    *result = gc_new_xfloat(dx);
    return TRUE;
  }
  if (bi_int64(argv->cons.car, &iy))
    return double_multiply(dx * iy, argv->cons.cdr, result);
  if (bi_double(argv->cons.car, &dy))
    return double_multiply(dx * dy, argv->cons.cdr, result);
  return mark_required_number();
}

static int int64_multiply(int64_t ix, object argv, object *result)
{
  int64_t iy;
  if (argv == object_nil) {
    *result = gc_new_xint(ix);
    return TRUE;
  }
  if (bi_int64(argv->cons.car, &iy)) {
    if (ix > 0) {
      if (iy > 0) {
        if (ix > INT64_MAX / iy) return mark_numeric_over_flow();
      } else {
        if (iy < INT64_MIN / ix) return mark_numeric_over_flow();
      }
    } else {
      if (iy > 0) {
        if (ix < INT64_MIN / iy) return mark_numeric_over_flow();
      } else {
        if (ix != 0 && iy < INT64_MAX / ix) return mark_numeric_over_flow();
      }
    }
    return int64_multiply(ix * iy, argv->cons.cdr, result);
  }
  return double_multiply((double)ix, argv, result);
}

DEFUN(_2a_)
{
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  return int64_multiply(1, argv, result);
}

static int double_divide(double dx, object argv, object *result)
{
  int64_t iy;
  double dy;
  if (!isfinite(dx)) return mark_numeric_over_flow();
  if (argv == object_nil) {
    *result = gc_new_xfloat(dx);
    return TRUE;
  }
  if (bi_int64(argv->cons.car, &iy)) dy = (double)iy;
  else if (!bi_double(argv->cons.car, &dy)) return mark_required_number();
  if (dy == 0) return mark_division_by_zero();
  return double_divide(dx / dy, argv->cons.cdr, result);
}

static int int64_divide(int64_t ix, object argv, object *result)
{
  int64_t iy;
  if (argv == object_nil) {
    *result = gc_new_xint(ix);
    return TRUE;
  }
  if (bi_int64(argv->cons.car, &iy)) {
    if (iy == 0) return mark_division_by_zero();
    if (ix == INT64_MIN && iy == -1) return mark_numeric_over_flow();
    if (ix % iy == 0) return int64_divide(ix / iy, argv->cons.cdr, result);
    return double_divide((double)ix/iy, argv->cons.cdr, result);
  }
  return double_divide((double)ix, argv, result);
}

DEFUN(_2f_)
{
  int64_t ix;
  double dx;
  object o;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  if (argc == 1) return int64_divide(1, argv, result);
  o = argv->cons.car;
  if (bi_int64(o, &ix)) return int64_divide(ix, argv->cons.cdr, result);
  if (bi_double(o, &dx)) return double_divide(dx, argv->cons.cdr, result);
  return mark_required_number();
}

#define DBL_MAX_INT ((int64_t)1<<DBL_MANT_DIG)
#define DBL_MIN_INT (-DBL_MAX_INT-1)

DEFUN(_2f__2f_)
{
  int64_t ix, iy;
  double dx;
  if (!bi_argc_range(argc, 1, 2)) return FALSE;
  if (argc == 1) {
    if (bi_int64(argv->cons.car, &ix)) {
      *result = argv->cons.car;
      return TRUE;
    }
    if (bi_double(argv->cons.car, &dx)) {
      if ((double)DBL_MIN_INT <= dx && dx <= (double)DBL_MAX_INT) {
        *result = gc_new_xint((int64_t)dx);
        return TRUE;
      }
    }
    return mark_numeric_over_flow();
  } else {
    if (!bi_int64(argv->cons.car, &ix)) return mark_required_integer();
    if (!bi_int64(argv->cons.cdr->cons.car, &iy)) return mark_required_integer();
    if (iy == 0) return mark_division_by_zero();
    *result = gc_new_xint(ix / iy);
    return TRUE;
  }
}

DEFUN(_25_)
{
  int64_t x, y;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_int64(argv->cons.car, &x)) return mark_required_integer();
  if (!bi_int64(argv->cons.cdr->cons.car, &y)) return mark_required_integer();
  if (y == 0) return mark_division_by_zero();
  *result = gc_new_xint(x % y);
  return TRUE;
}

static int double_lt(double x, object argv, object *result)
{
  int64_t i;
  double d;
  if (argv == object_nil) {
    *result = object_true;
    return TRUE;
  }
  if (bi_int64(argv->cons.car, &i)) {
    if (x >= (double)i) return TRUE;
    return double_lt((double)i, argv->cons.cdr, result);
  }
  if (bi_double(argv->cons.car, &d)) {
    if (x >= d) return TRUE;
    return double_lt(d, argv->cons.cdr, result);
  }
  return mark_required_number();
}

static int int64_lt(int64_t x, object argv, object *result)
{
  int64_t y;
  if (argv == object_nil) {
    *result = object_true;
    return TRUE;
  }
  if (bi_int64(argv->cons.car, &y)) {
    if (x >= y) return TRUE;
    return int64_lt(y, argv->cons.cdr, result);
  }
  return double_lt((double)x, argv, result);
}

DEFUN(_3c_)
{
  int64_t i;
  double d;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  *result = object_nil;
  if (bi_int64(argv->cons.car, &i))
    return int64_lt(i, argv->cons.cdr, result);
  if (bi_double(argv->cons.car, &d))
    return double_lt(d, argv->cons.cdr, result);
  return mark_required_number();
}

DEFUN(_7e_)
{
  int64_t x;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_int64(argv->cons.car, &x) || x < 0)
    return mark_required_positive_int();
  *result = gc_new_xint(~x & INT64_MAX);
  return TRUE;
}

DEFUN(_26_)
{
  int64_t x, y;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_int64(argv->cons.car, &x) || x < 0)
    return mark_required_positive_int();
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!bi_int64(argv->cons.car, &y) || y < 0)
      return mark_required_positive_int();
    x &= y;
  }
  *result = gc_new_xint(x);
  return TRUE;
}

DEFUN(_7c_)
{
  int64_t x, y;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_int64(argv->cons.car, &x) || x < 0)
    return mark_required_positive_int();
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!bi_int64(argv->cons.car, &y) || y < 0)
      return mark_required_positive_int();
    x |= y;
  }
  *result = gc_new_xint(x);
  return TRUE;
}

DEFUN(_5e_)
{
  int64_t x, y;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_int64(argv->cons.car, &x) || x < 0)
    return mark_required_positive_int();
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!bi_int64(argv->cons.car, &y) || y < 0)
      return mark_required_positive_int();
    x ^= y;
  }
  *result = gc_new_xint(x);
  return TRUE;
}

static int bits(int64_t x)
{
  int i;
  for (i = 0; i < XINT_BITS; i++)
    if (x < (1LL << i)) return i;
  return XINT_BITS;
}

DEFUN(_3c__3c_)
{
  int64_t x, y;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_int64(argv->cons.car, &x) || x < 0)
    return mark_required_positive_int();
  if (!bi_int64(argv->cons.cdr->cons.car, &y)) return mark_required_integer();
  if (x != 0) {
    if (y > 0) {
      if ((bits(x) + y) > XINT_BITS) return mark_numeric_over_flow();
      x <<= y;
    } else x >>= -y;
  }
  *result = gc_new_xint(x);
  return TRUE;
}
