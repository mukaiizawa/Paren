// built-in.

#include "std.h"
#include "xsleep.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "pf.h"
#include "ip.h"

struct xbarray bi_buf;

int bi_argc_range(int argc, int min, int max)
{
  if (argc < min)
    return ip_throw(ArgumentError, too_few_arguments);
  if ((!min && !max && argc != 0) || (max && argc > max))
    return ip_throw(ArgumentError, too_many_arguments);
  return TRUE;
}

static int type_bits(object o)
{
  switch (object_type(o)) {
    case SINT:
    case XINT:
    case XFLOAT:
      return BI_NUM;
    case CONS:
      return BI_LIST | BI_CONS;
    case SYMBOL:
      if (o == object_nil) return BI_SYM | BI_LIST;
      return BI_SYM;
    case KEYWORD:
      return BI_KEY;
    case STRING:
      return BI_STR;
    case BYTES:
      return BI_BYTES;
    case ARRAY:
      return BI_ARRAY;
    case DICT:
      return BI_DICT;
    case MACRO:
      return BI_MACRO;
    case FUNC:
      return BI_FUNC;
    case SPECIAL:
      return BI_SP;
    case BFUNC:
      return BI_FUNC;
    default:
      xassert(FALSE);
      return 0;
  }
}

static char *type_name(int type) {
  switch (type) {
    case BI_SYM: return "symbol";
    case BI_STR: return "string";
    case BI_ARRAY: return "array";
    case BI_BYTES: return "bytes";
    case BI_CONS: return "cons";
    case BI_DICT: return "dictionary";
    case BI_FUNC: return "function";
    case BI_MACRO: return "macro";
    case BI_SP: return "special operator";
    case BI_KEY: return "keyword";
    case BI_LIST: return "list";
    case BI_NUM: return "number";
    default: xassert(FALSE); return NULL;
  }
}

#define CHECK(t) {\
  if (t & bits) {\
    if (n != 1) {\
      if (i != 0) xbarray_adds(&bi_buf, ", ");\
      else if (i == n - 1) xbarray_adds(&bi_buf, " or ");\
    }\
    xbarray_adds(&bi_buf, type_name(t));\
    i++;\
  }\
}

static int bit_count (int bits) {
  bits = (bits & 0x55555555) + (bits >> 1 & 0x55555555);
  bits = (bits & 0x33333333) + (bits >> 2 & 0x33333333);
  bits = (bits & 0x0f0f0f0f) + (bits >> 4 & 0x0f0f0f0f);
  bits = (bits & 0x00ff00ff) + (bits >> 8 & 0x00ff00ff);
  return (bits & 0x0000ffff) + (bits >>16 & 0x0000ffff);
}

int bi_argv(int bits, object o, object *result)
{
  int i, n;
  *result = o;
  if (type_bits(o) & bits) return TRUE;
  i = 0, n = bit_count(bits);
  xbarray_reset(&bi_buf);
  xbarray_adds(&bi_buf, "expected ");
  CHECK(BI_SYM);
  CHECK(BI_STR);
  CHECK(BI_ARRAY);
  CHECK(BI_BYTES);
  CHECK(BI_CONS);
  CHECK(BI_DICT);
  CHECK(BI_FUNC);
  CHECK(BI_MACRO);
  CHECK(BI_SP);
  CHECK(BI_KEY);
  CHECK(BI_LIST);
  CHECK(BI_NUM);
  xbarray_add(&bi_buf, '\0');
  return ip_throw(ArgumentError, bi_buf_msg);
}

int bi_range(int min, int x, int max)
{
  if (x < min || x > max)
    return ip_throw(IndexError, index_out_of_range);
  return TRUE;
}

int bi_cbyte(object o, int *p)
{
  if (!bi_cint(o, p) || *p < 0 || *p > 0xff)
    return ip_throw(ArgumentError, expected_byte);
  return TRUE;
}

int bi_cint(object o, int *p)
{
  *p = sint_val(o);
  if (!sint_p(o))
    return ip_throw(ArgumentError, expected_integer);
  return TRUE;
}

int bi_cpint(object o, int *p)
{
  if (!bi_cint(o, p) || *p < 0)
    return ip_throw(ArgumentError, expected_positive_integer);
  return TRUE;
}

int bi_cint64(object o, int64_t *p)
{
  if (sint_p(o)) *p = sint_val(o);
  else if (object_type(o) == XINT) *p = o->xint.val;
  else return FALSE;
  return TRUE;
}

int bi_cintptr(object o, intptr_t *p)
{
  int64_t i;
  if (bi_cint64(o, &i)) {
    if (INTPTR_MIN <= i && i <= INTPTR_MAX) {
      *p = (intptr_t)i;
      return TRUE;
    }
  }
  *p = (intptr_t)0;
  return ip_throw(ArgumentError, expected_integer);
}

int bi_cdouble(object o, double *p)
{
  int64_t i;
  if (bi_cint64(o, &i)) {
    *p = (double)i;
    return TRUE;
  }
  if (object_type(o) == XFLOAT) {
    *p = o->xfloat.val;
    return TRUE;
  }
  return FALSE;
}

#define MAX_STRINGS 2

int bi_cstrings(int n, object argv, char **ss)
{
  int i, offset[MAX_STRINGS]; // xbarray use realloc.
  object o;
  xassert(n <= MAX_STRINGS);
  xassert(object_type(argv) == CONS);
  xbarray_reset(&bi_buf);
  for (i = 0; i < n; i++) {
    if (!bi_argv(BI_STR, argv->cons.car, &o)) return FALSE;
    argv = argv->cons.cdr;
    offset[i] = bi_buf.size;
    memcpy(xbarray_reserve(&bi_buf, o->mem.size), o->mem.elt, o->mem.size);
    xbarray_add(&bi_buf, '\0');
  }
  for (i = 0; i < n; i++) ss[i] = bi_buf.elt + offset[i];
  return TRUE;
}

int bi_cstring(object argv, char **ss)
{
  char *result;
  if (!bi_cstrings(1, argv, &result)) return FALSE;
  *ss = result;
  return TRUE;
}

// fundamental functions.

static int bi_type(int type, int argc, object argv, object *result)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type(argv->cons.car) == type);
  return TRUE;
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

DEFUN(cons_3f_)
{
  return bi_type(CONS, argc, argv, result);
}

DEFUN(symbol_3f_)
{
  return bi_type(SYMBOL, argc, argv, result);
}

DEFUN(keyword_3f_)
{
  return bi_type(KEYWORD, argc, argv, result);
}

DEFUN(string_3f_)
{
  return bi_type(STRING, argc, argv, result);
}

DEFUN(bytes_3f_)
{
  return bi_type(BYTES, argc, argv, result);
}

DEFUN(array_3f_)
{
  return bi_type(ARRAY, argc, argv, result);
}

DEFUN(dict_3f_)
{
  return bi_type(DICT, argc, argv, result);
}

DEFUN(macro_3f_)
{
  return bi_type(MACRO, argc, argv, result);
}

DEFUN(special_2d_operator_3f_)
{
  return bi_type(SPECIAL, argc, argv, result);
}

DEFUN(function_3f_)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  switch (object_type(argv->cons.car)) {
    case BFUNC:
    case FUNC:
      *result = object_bool(TRUE);
      break;
    default:
      *result = object_bool(FALSE);
      break;
  }
  return TRUE;
}

DEFUN(hash)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = gc_new_xint(object_hash(argv->cons.car));
  return TRUE;
}

DEFUN(address)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = gc_new_xint((intptr_t)argv->cons.car);
  return TRUE;
}

DEFUN(_21_)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(argv->cons.car == object_nil);
  return TRUE;
}

DEFUN(_3d_)
{
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  *result = object_bool(object_eq_p(argv->cons.car, argv->cons.cdr->cons.car));
  return TRUE;
}

DEFUN(_3d__3d_)
{
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  *result = object_bool(argv->cons.car == argv->cons.cdr->cons.car);
  return TRUE;
}

DEFUN(built_2d_in_3f_)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  switch (object_type(argv->cons.car)) {
    case BFUNC:
    case SPECIAL:
      *result = object_true;
      break;
    default:
      *result = object_nil;
      break;
  }
  return TRUE;
}

DEFUN(built_2d_in_2d_name)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  switch (object_type(argv->cons.car)) {
    case BFUNC:
    case SPECIAL:
      *result = argv->cons.car->native.name;
      break;
    default:
      *result = object_nil;
      break;
  }
  return TRUE;
}

DEFUN(params)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_FUNC | BI_MACRO, argv->cons.car, &o)) return FALSE;
  switch (object_type(o)) {
    case BFUNC:
      *result = object_nil;
      break;
    case MACRO:
    case FUNC:
      *result = o->proc.params;
      break;
    default:
      xassert(FALSE);
      break;
  }
  return TRUE;
}

DEFUN(body)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_FUNC | BI_MACRO, argv->cons.car, &o)) return FALSE;
  switch (object_type(o)) {
    case BFUNC:
      *result = object_nil;
      break;
    case MACRO:
    case FUNC:
      *result = o->proc.body;
      break;
    default:
      xassert(FALSE);
      break;
  }
  return TRUE;
}

// list.

DEFUN(cons)
{
  object o;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.cdr->cons.car, &o)) return FALSE;
  *result = gc_new_cons(argv->cons.car, o);
  return TRUE;
}

DEFUN(list)
{
  *result = argv;
  return TRUE;
}

DEFUN(list_2e__2e__2e_)
{
  int i;
  object o, p;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_LIST | BI_BYTES | BI_STR | BI_ARRAY | BI_DICT, argv->cons.car, &o)) return FALSE;
  *result = object_nil;
  switch (object_type(o)) {
    case SYMBOL:
      break;
    case CONS:
      while (o != object_nil) {
        *result = gc_new_cons(o->cons.car, *result);
        o = o->cons.cdr;
      }
      break;
    case STRING:
      i = 0;
      while (i < o->mem.size) {
        if (!ch_at(o, &i, &p)) return FALSE;
        *result = gc_new_cons(p, *result);
      }
      break;
    case BYTES:
      for (i = 0; i < o->mem.size; i++)
        *result = gc_new_cons(gc_new_xint(LC(o->mem.elt + i)), *result);
      break;
    case ARRAY:
      for (i = 0; i < o->array.size; i++)
        *result = gc_new_cons(o->array.elt[i], *result);
      break;
    default:
      xassert(FALSE);
      return FALSE;
  }
  *result = list_reverse(*result);
  return TRUE;
}

DEFUN(car)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.car;
  return TRUE;
}

DEFUN(car_21_)
{
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_argv(BI_CONS, argv->cons.car, &o)) return FALSE;
  p = argv->cons.cdr->cons.car;
  o->cons.car = p;
  *result = p;
  return TRUE;
}

DEFUN(cdr)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.cdr;
  return TRUE;
}

DEFUN(cdr_21_)
{
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_argv(BI_CONS, argv->cons.car, &o)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.cdr->cons.car, &p)) return FALSE;
  o->cons.cdr = p;
  *result = p;
  return TRUE;
}

DEFUN(last_2d_cons)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.car, &o)) return FALSE;
  if (o != object_nil) {
    while (o->cons.cdr != object_nil) o = o->cons.cdr;
  }
  *result = o;
  return TRUE;
}

DEFUN(reverse_21_)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = list_reverse(o);
  return TRUE;
}

// number.

static int double_add(double x, object argv, object *result)
{
  int64_t i;
  double d;
  if (!isfinite(x))
    return ip_throw(ArithmeticError, numeric_overflow);
  if (argv == object_nil) {
    *result = gc_new_xfloat(x);
    return TRUE;
  }
  if (bi_cint64(argv->cons.car, &i))
    return double_add(x + (double)i, argv->cons.cdr, result);
  if (bi_cdouble(argv->cons.car, &d))
    return double_add(x + d, argv->cons.cdr, result);
  return ip_throw(ArgumentError, expected_number);
}

static int int64_add(int64_t x, object argv, object *result)
{
  int64_t y;
  if (argv == object_nil) {
    *result = gc_new_xint(x);
    return TRUE;
  }
  if (bi_cint64(argv->cons.car, &y)) {
    if ((y > 0 && x > INT64_MAX - y) || (y < 0 && x < INT64_MIN - y))
      return ip_throw(ArithmeticError, numeric_overflow);
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
  if (!isfinite(dx))
    return ip_throw(ArithmeticError, numeric_overflow);
  if (argv == object_nil) {
    *result = gc_new_xfloat(dx);
    return TRUE;
  }
  if (bi_cint64(argv->cons.car, &iy))
    return double_multiply(dx * iy, argv->cons.cdr, result);
  if (bi_cdouble(argv->cons.car, &dy))
    return double_multiply(dx * dy, argv->cons.cdr, result);
  return ip_throw(ArgumentError, expected_number);
}

static int int64_multiply(int64_t ix, object argv, object *result)
{
  int64_t iy;
  if (argv == object_nil) {
    *result = gc_new_xint(ix);
    return TRUE;
  }
  if (bi_cint64(argv->cons.car, &iy)) {
    if (ix > 0) {
      if (iy > 0) {
        if (ix > INT64_MAX / iy)
          return ip_throw(ArithmeticError, numeric_overflow);
      } else {
        if (iy < INT64_MIN / ix)
          return ip_throw(ArithmeticError, numeric_overflow);
      }
    } else {
      if (iy > 0) {
        if (ix < INT64_MIN / iy)
          return ip_throw(ArithmeticError, numeric_overflow);
      } else {
        if (ix != 0 && iy < INT64_MAX / ix)
          return ip_throw(ArithmeticError, numeric_overflow);
      }
    }
    return int64_multiply(ix * iy, argv->cons.cdr, result);
  }
  return double_multiply((double)ix, argv, result);
}

DEFUN(_2a_)
{
  if (argc == 0) {
    *result = gc_new_xint(1);
    return TRUE;
  }
  return int64_multiply(1, argv, result);
}

static int double_divide(double dx, object argv, object *result)
{
  int64_t iy;
  double dy;
  if (!isfinite(dx))
    return ip_throw(ArithmeticError, numeric_overflow);
  if (argv == object_nil) {
    *result = gc_new_xfloat(dx);
    return TRUE;
  }
  if (bi_cint64(argv->cons.car, &iy)) dy = (double)iy;
  else if (!bi_cdouble(argv->cons.car, &dy))
    return ip_throw(ArgumentError, expected_number);
  if (dy == 0)
    return ip_throw(ArithmeticError, division_by_zero);
  return double_divide(dx / dy, argv->cons.cdr, result);
}

static int int64_divide(int64_t ix, object argv, object *result)
{
  int64_t iy;
  if (argv == object_nil) {
    *result = gc_new_xint(ix);
    return TRUE;
  }
  if (bi_cint64(argv->cons.car, &iy)) {
    if (iy == 0)
      return ip_throw(ArithmeticError, division_by_zero);
    if (ix == INT64_MIN && iy == -1)
      return ip_throw(ArithmeticError, numeric_overflow);
    if (ix % iy == 0)
      return int64_divide(ix / iy, argv->cons.cdr, result);
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
  if (bi_cint64(o, &ix)) return int64_divide(ix, argv->cons.cdr, result);
  if (bi_cdouble(o, &dx)) return double_divide(dx, argv->cons.cdr, result);
  return ip_throw(ArgumentError, expected_number);
}

DEFUN(_2f__2f_)
{
  int64_t ix, iy;
  double dx;
  if (!bi_argc_range(argc, 1, 2)) return FALSE;
  if (argc == 1) {
    if (bi_cint64(argv->cons.car, &ix)) {
      *result = argv->cons.car;
      return TRUE;
    }
    if (bi_cdouble(argv->cons.car, &dx)) {
      if ((double)DBL_MIN_INT <= dx && dx <= (double)DBL_MAX_INT) {
        *result = gc_new_xint((int64_t)dx);
        return TRUE;
      }
    }
    return ip_throw(ArithmeticError, numeric_overflow);
  } else {
    if (!bi_cint64(argv->cons.car, &ix))
      return ip_throw(ArgumentError, expected_integer);
    if (!bi_cint64(argv->cons.cdr->cons.car, &iy))
      return ip_throw(ArgumentError, expected_integer);
    if (iy == 0)
      return ip_throw(ArithmeticError, division_by_zero);
    *result = gc_new_xint(ix / iy);
    return TRUE;
  }
}

DEFUN(_25_)
{
  int64_t x, y;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cint64(argv->cons.car, &x))
    return ip_throw(ArgumentError, expected_integer);
  if (!bi_cint64(argv->cons.cdr->cons.car, &y))
    return ip_throw(ArgumentError, expected_integer);
  if (y == 0)
    return ip_throw(ArithmeticError, division_by_zero);
  *result = gc_new_xint(x % y);
  return TRUE;
}

// bitwise operator.

DEFUN(_7e_)
{
  int64_t x;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cint64(argv->cons.car, &x) || x < 0)
    return ip_throw(ArgumentError, expected_positive_integer);
  *result = gc_new_xint(~x & INT64_MAX);
  return TRUE;
}

DEFUN(_26_)
{
  int64_t x, y;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_cint64(argv->cons.car, &x) || x < 0)
    return ip_throw(ArgumentError, expected_positive_integer);
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!bi_cint64(argv->cons.car, &y) || y < 0)
      return ip_throw(ArgumentError, expected_positive_integer);
    x &= y;
  }
  *result = gc_new_xint(x);
  return TRUE;
}

DEFUN(_7c_)
{
  int64_t x, y;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_cint64(argv->cons.car, &x) || x < 0)
    return ip_throw(ArgumentError, expected_positive_integer);
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!bi_cint64(argv->cons.car, &y) || y < 0)
      return ip_throw(ArgumentError, expected_positive_integer);
    x |= y;
  }
  *result = gc_new_xint(x);
  return TRUE;
}

DEFUN(_5e_)
{
  int64_t x, y;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_cint64(argv->cons.car, &x) || x < 0)
    return ip_throw(ArgumentError, expected_positive_integer);
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!bi_cint64(argv->cons.car, &y) || y < 0)
      return ip_throw(ArgumentError, expected_positive_integer);
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
  if (!bi_cint64(argv->cons.car, &x) || x < 0)
    return ip_throw(ArgumentError, expected_positive_integer);
  if (!bi_cint64(argv->cons.cdr->cons.car, &y))
    return ip_throw(ArgumentError, expected_integer);
  if (x != 0) {
    if (y > 0) {
      if ((bits(x) + y) > XINT_BITS)
        return ip_throw(ArithmeticError, numeric_overflow);
      x <<= y;
    } else x >>= -y;
  }
  *result = gc_new_xint(x);
  return TRUE;
}

// mathematical functions.

static int xmath(int argc, object argv, double (*f)(double c), object *result)
{
  double x;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cdouble(argv->cons.car, &x)) return ip_throw(ArgumentError, expected_number);
  if (!isfinite(x = f(x))) return ip_throw(ArithmeticError, numeric_overflow);
  *result = gc_new_xfloat(x);
  return TRUE;
}

DEFUN(sin)
{
  return xmath(argc, argv, sin, result);
}

DEFUN(cos)
{
  return xmath(argc, argv, cos, result);
}

DEFUN(tan)
{
  return xmath(argc, argv, tan, result);
}

DEFUN(asin)
{
  return xmath(argc, argv, asin, result);
}

DEFUN(acos)
{
  return xmath(argc, argv, acos, result);
}

DEFUN(atan)
{
  return xmath(argc, argv, atan, result);
}

DEFUN(sinh)
{
  return xmath(argc, argv, sinh, result);
}

DEFUN(cosh)
{
  return xmath(argc, argv, cosh, result);
}

DEFUN(tanh)
{
  return xmath(argc, argv, tanh, result);
}

DEFUN(exp)
{
  return xmath(argc, argv, exp, result);
}

DEFUN(log)
{
  double x, y;
  if (!bi_argc_range(argc, 1, 2)) return FALSE;
  if (argc == 1) return xmath(argc, argv, log, result);
  if (!bi_cdouble(argv->cons.car, &x)) return ip_throw(ArgumentError, expected_number);
  if (!bi_cdouble(argv->cons.cdr->cons.car, &y)) return ip_throw(ArgumentError, expected_number);
  if (!isfinite(x = log(y) / log(x))) return ip_throw(ArithmeticError, numeric_overflow);
  *result = gc_new_xfloat(x);
  return TRUE;
}

DEFUN(sqrt)
{
  return xmath(argc, argv, sqrt, result);
}

DEFUN(pow)
{
  double x, y;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cdouble(argv->cons.car, &x)) return ip_throw(ArgumentError, expected_number);
  if (!bi_cdouble(argv->cons.cdr->cons.car, &y)) return ip_throw(ArgumentError, expected_number);
  if (!isfinite(x = pow(x, y))) return ip_throw(ArithmeticError, numeric_overflow);
  *result = gc_new_xfloat(x);
  return TRUE;
}

// bytes

static int bytes_like_to(int type, int argc, object argv, object *result)
{
  int start, stop;
  object o;
  if (!bi_argc_range(argc, 1, 3)) return FALSE;
  if (!bi_argv(BI_BYTES | BI_STR | BI_SYM | BI_KEY, argv->cons.car, &o)) return FALSE;
  if (argc < 2) start = 0;
  else if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &start)) return FALSE;
  if (argc < 3) stop = o->mem.size;
  else if (!bi_cint(argv->cons.cdr->cons.car, &stop)) return FALSE;
  if (!bi_range(start, stop, o->mem.size)) return FALSE;
  *result = gc_new_mem_from(type, o->mem.elt + start, stop - start);
  return TRUE;
}

DEFUN(bytes)
{
  int i;
  if (argc == 1 && object_type(argv->cons.car) == SINT) {
    if (!bi_cpint(argv->cons.car, &i)) return FALSE;
    *result = gc_new_mem(BYTES, i);
    return TRUE;
  }
  return bytes_like_to(BYTES, argc, argv, result);
}

DEFUN(symbol)
{
  static int c = 0;
  if (argc == 0) {
    xbarray_reset(&bi_buf);
    xbarray_addf(&bi_buf, "$G-%d", ++c);
    *result = gc_new_mem_from(SYMBOL, bi_buf.elt, bi_buf.size);
    return TRUE;
  }
  return bytes_like_to(SYMBOL, argc, argv, result);
}

DEFUN(keyword)
{
  return bytes_like_to(KEYWORD, argc, argv, result);
}

DEFUN(string)
{
  return bytes_like_to(STRING, argc, argv, result);
}

DEFUN(string_21_)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_BYTES, argv->cons.car, &o)) return FALSE;
  object_reset_type(o, STRING);
  object_set_hash(o, object_mem_hash(o->mem.elt, o->mem.size));
  *result = o;
  return TRUE;
}

DEFUN(memcpy)
{
  int oi, pi, size;
  object o, p;
  if (!bi_argc_range(argc, 5, 5)) return FALSE;
  if (!bi_argv(BI_BYTES | BI_STR | BI_SYM | BI_KEY, argv->cons.car, &o)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &oi)) return FALSE;
  if (!bi_argv(BI_BYTES, (argv = argv->cons.cdr)->cons.car, &p)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &pi)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!bi_range(0, oi + size, o->mem.size)) return FALSE;
  if (!bi_range(0, pi + size, p->mem.size)) return FALSE;
  memmove(p->mem.elt + pi, o->mem.elt + oi, size);
  *result = p;
  return TRUE;
}

DEFUN(byte_2d_len)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_BYTES | BI_STR | BI_SYM | BI_KEY, argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint(o->mem.size);
  return TRUE;
}

DEFUN(memcmp)
{
  int val, size;
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_argv(BI_BYTES | BI_STR | BI_SYM | BI_KEY, argv->cons.car, &o)) return FALSE;
  if (!bi_argv(BI_BYTES | BI_STR | BI_SYM | BI_KEY, argv->cons.cdr->cons.car, &p)) return FALSE;
  size = o->mem.size;
  if (size > p->mem.size) size = p->mem.size;
  if ((val = memcmp(o->mem.elt, p->mem.elt, size)) == 0) {
    if (o->mem.size > p->mem.size) val = 1;
    else if (o->mem.size < p->mem.size) val = -1;
  }
  else if (val > 0) val = 1;    // normalize
  else val = -1;
  *result = gc_new_xint(val);
  return TRUE;
}

static int bytes_like_concat(object o, object argv, object *result)
{
  object p;
  struct xbarray x;
  xbarray_init(&x);
  xbarray_copy(&x, o->mem.elt, o->mem.size);
  while (argv != object_nil) {
    if (!bi_argv(BI_BYTES | BI_STR | BI_SYM | BI_KEY, argv->cons.car, &p)) {
      xbarray_free(&x);
      return FALSE;
    }
    xbarray_copy(&x, p->mem.elt, p->mem.size);
    argv = argv->cons.cdr;
  }
  *result = gc_new_mem_from(object_type(o), x.elt, x.size);
  xbarray_free(&x);
  return TRUE;
}

DEFUN(chr)
{
  int x, size;
  char buf[4];
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cpint(argv->cons.car, &x)) return FALSE;
  if (x <= 0x7f) {
    size = 1;
    buf[0] = x;
  } else if (x <= 0x7ff) {
    size = 2;
    buf[0] = 0xc0 | ((x >> 6) & 0x3f);
    buf[1] = 0x80 | (x & 0x3f);
  } else if (x <= 0xffff) {
    size = 3;
    buf[0] = 0xe0 | ((x >> 12) & 0x3f);
    buf[1] = 0x80 | ((x >> 6) & 0x3f);
    buf[2] = 0x80 | (x & 0x3f);
  } else if (x <= 0xfffff) {
    size = 4;
    buf[0] = 0xf0 | ((x >> 18) & 0x3f);
    buf[1] = 0x80 | ((x >> 12) & 0x3f);
    buf[2] = 0x80 | ((x >> 6) & 0x3f);
    buf[3] = 0x80 | (x & 0x3f);
  } else return ip_throw(IndexError, index_out_of_range);
  *result = gc_new_mem_from(STRING, buf, size);
  return TRUE;
}

static int ord(object o, int from, int len, int *val)
{
  xassert(object_type(o) == STRING);
  if (from + len > o->mem.size) return ip_throw(ArgumentError, incomplete_utf8_byte_sequence);
  switch (len) {
    case 1:
      *val = LC(o->mem.elt + from);
      return TRUE;
    case 2:
      *val = ((LC(o->mem.elt + from) & 0x3f) << 6)
        | (LC(o->mem.elt + from + 1) & 0x3f);
      return TRUE;
    case 3:
      *val = ((LC(o->mem.elt + from) & 0xf) << 12)
        | ((LC(o->mem.elt + from + 1) & 0x3f) << 6)
        | (LC(o->mem.elt + from + 2) & 0x3f);
      return TRUE;
    case 4:
      *val = ((LC(o->mem.elt + from) & 0x3) << 18)
        | ((LC(o->mem.elt + from + 1) & 0x3f) << 12)
        | ((LC(o->mem.elt + from + 2) & 0x3f) << 6)
        | (LC(o->mem.elt + from + 3) & 0x3f);
      return TRUE;
    default:
      *val = -1;
      return ip_throw(ArgumentError, incomplete_utf8_byte_sequence);
  }
}

DEFUN(ord)
{
  int x;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_STR, argv->cons.car, &o)) return FALSE;
  if (!ord(o, 0, o->mem.size, &x)) return FALSE;
  *result = gc_new_xint(x);
  return TRUE;
}

DEFUN(ascii_3f_)
{
  int len;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_STR, argv->cons.car, &o)) return FALSE;
  if (!str_len(o, &len)) return FALSE;
  *result = object_bool(len == o->mem.size);
  return TRUE;
}

static int xctype_p(int argc, object argv, int (*f)(int c), object *result)
{
  int i;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_STR, argv->cons.car, &o)) return FALSE;
  *result = object_nil;
  if (o->mem.size == 0) return TRUE;
  i = 0;
  while (i < o->mem.size) {
    if (!f(LC(o->mem.elt + i))) return TRUE;
    if (!ch_len(LC(o->mem.elt + i), &i)) return FALSE;
  }
  *result = object_true;
  return TRUE;
}

DEFUN(alnum_3f_)
{
  return xctype_p(argc, argv, isalnum, result);
}

DEFUN(alpha_3f_)
{
  return xctype_p(argc, argv, isalpha, result);
}

DEFUN(digit_3f_)
{
  return xctype_p(argc, argv, isdigit, result);
}

DEFUN(space_3f_)
{
  return xctype_p(argc, argv, isspace, result);
}

DEFUN(print_3f_)
{
  return xctype_p(argc, argv, isprint, result);
}

DEFUN(lower_3f_)
{
  return xctype_p(argc, argv, islower, result);
}

DEFUN(upper_3f_)
{
  return xctype_p(argc, argv, isupper, result);
}

static int ch_conv_case(int argc, object argv, int (*f)(int c), int offset, object *result)
{
  int i, ch;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_STR, argv->cons.car, &o)) return FALSE;
  o = gc_new_mem_from(STRING, o->mem.elt, o->mem.size);
  i = 0;
  while (i < o->mem.size) {
    if (f(ch = LC(o->mem.elt + i))) SC(o->mem.elt + i, ch + offset);
    if (!ch_len(ch, &i)) return FALSE;
  }
  *result = o;
  return TRUE;
}

DEFUN(lower)
{
  return ch_conv_case(argc, argv, isupper, 0x20, result);
}

DEFUN(upper)
{
  return ch_conv_case(argc, argv, islower, -0x20, result);
}

// array.

static int string_to_array(object o, object *result)
{
  int i, j, size;
  if (!str_len(o, &size)) return FALSE;
  *result = gc_new_array(size);
  for (i = 0, j = 0; i < size; i++)
    ch_at(o, &j, ((*result)->array.elt + i));
  xassert(j == o->mem.size);
  return TRUE;
}

DEFUN(array)
{
  int i, size;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if ((o = argv->cons.car) == object_nil) {
    *result = gc_new_array(0);
    return TRUE;
  }
  switch (object_type(o)) {
    case SINT:
    case XINT:
      if (!bi_cpint(o, &size)) return FALSE;
      *result = gc_new_array(size);
      return TRUE;
    case CONS:
      size = list_len(o);
      *result = gc_new_array(size);
      for (i = 0; i < size; i++) {
        (*result)->array.elt[i] = o->cons.car;
        o = o->cons.cdr;
      }
      xassert(o == object_nil);
      return TRUE;
    case STRING:
      return string_to_array(o, result);
    case BYTES:
      size = o->mem.size;
      *result = gc_new_array(o->mem.size);
      for (i = 0; i < size; i++)
        (*result)->array.elt[i] = gc_new_xint(o->mem.elt[i]);
      return TRUE;
    case ARRAY:
      *result = gc_new_array_from(o->array.elt, o->array.size);
      return TRUE;
    default:
      return ip_throw(ArgumentError, expected_positive_integer_or_sequence);
  }
}

// dictionary.

DEFUN(dict)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  *result = gc_new_dict();
  return TRUE;
}

DEFUN(keys)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_DICT, argv->cons.car, &o)) return FALSE;
  *result = map_keys(o);
  return TRUE;
}

static int cons_slice(object o, int start, int stop, object *result)
{
  int i;
  object tail;
  for (i = 0; i < start; i++)
    if ((o = o->cons.cdr) == object_nil) break;
  if (o == object_nil) {
    *result = object_nil;
    return TRUE;
  }
  *result = tail = gc_copy_cons(o, &tail);
  if (stop != -1) {
    if (start == stop) *result = object_nil;
    else {
      for (i = start + 1; i < stop; i++)
        if ((tail = tail->cons.cdr) == object_nil) return TRUE;
      tail->cons.cdr = object_nil;
    }
  }
  return TRUE;
}

static int str_slice(object o, int start, int stop, object *result)
{
  int i, s, t;
  for (i = s = 0; i < start; i++)
    if (!ch_len(LC(o->mem.elt + s), &s)) return FALSE;
  if (stop == -1) t = o->mem.size;
  else {
    for (i = start, t = s; i < stop; i++)
      if (!ch_len(LC(o->mem.elt + t), &t)) return FALSE;
    if (t > o->mem.size) return ip_throw(ArgumentError, incomplete_utf8_byte_sequence);
  }
  if (!bi_range(0, s, t)) return FALSE;
  *result = gc_new_mem_from(STRING, o->mem.elt + s, t - s);
  return TRUE;
}

DEFUN(slice)
{
  int start, stop;
  object o;
  if (!bi_argc_range(argc, 1, 3)) return FALSE;
  if (!bi_argv(BI_LIST | BI_BYTES | BI_STR | BI_ARRAY, argv->cons.car, &o)) return FALSE;
  if (argc < 2) start = 0;
  else if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &start)) return FALSE;
  if (argc < 3) stop = -1;
  else if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &stop)) return FALSE;
  else if (!bi_range(0, start, stop)) return FALSE;
  *result = object_nil;
  switch (object_type(o)) {
    case SYMBOL:
      *result = object_nil;
      return TRUE;
    case CONS:
      return cons_slice(o, start, stop, result);
    case BYTES:
      if (stop == -1) stop = o->mem.size;
      if (!bi_range(start, stop, o->mem.size)) return FALSE;
      *result = gc_new_mem_from(BYTES, o->mem.elt + start, stop - start);
      return TRUE;
    case STRING:
      return str_slice(o, start, stop, result);
    case ARRAY:
      if (stop == -1) stop = o->array.size;
      if (!bi_range(start, stop, o->mem.size)) return FALSE;
      *result = gc_new_array_from(o->array.elt + start, stop - start);
      return TRUE;
    default:
      xassert(FALSE);
      return FALSE;
  }
}

static int cons_concat(object argv, object *result)
{
  object o;
  *result = object_nil;
  while (argv != object_nil) {
    if (!bi_argv(BI_LIST, argv->cons.car, &o)) return FALSE;
    while (o != object_nil) {
      *result = gc_new_cons(o->cons.car, *result);
      o = o->cons.cdr;
    }
    argv = argv->cons.cdr;
  }
  *result = list_reverse(*result);
  return TRUE;
}

static int array_concat(object o, object argv, object *result)
{
  int i, j, size;
  object p, q;
  size = o->array.size;
  p = argv;
  while (p != object_nil) {
    if (!bi_argv(BI_ARRAY, p->cons.car, &q)) return FALSE;
    size += q->array.size;
    p = p->cons.cdr;
  }
  *result = gc_new_array(size);
  for (i = 0, j = 0; j < o->array.size; i++, j++)
    (*result)->array.elt[i] = o->array.elt[j];
  while (argv != object_nil) {
    q = argv->cons.car;
    argv = argv->cons.cdr;
    for (j = 0; j < q->array.size; i++, j++)
      (*result)->array.elt[i] = q->array.elt[j];
  }
  return TRUE;
}

DEFUN(concat)
{
  object o;
  if (argc == 0) {
    *result = object_nil;
    return TRUE;
  }
  if (!bi_argv(BI_SYM | BI_KEY | BI_LIST | BI_BYTES | BI_STR | BI_ARRAY, argv->cons.car, &o)) return FALSE;
  if (o == object_nil) return cons_concat(argv, result);
  switch (object_type(o)) {
    case CONS:
      return cons_concat(argv, result);
    case SYMBOL:
    case KEYWORD:
    case BYTES:
    case STRING:
      return bytes_like_concat(o, argv->cons.cdr, result);
    case ARRAY:
      return array_concat(o, argv->cons.cdr, result);
    default:
      xassert(FALSE);
      return FALSE;
  }
}

static int cons_access(int argc, object argv, object o, object *result)
{
  int i;
  if (!bi_cpint(argv->cons.car, &i)) return FALSE;
  while (i-- > 0 && o != object_nil) o = o->cons.cdr;
  if (argc == 2) {
    if (o == object_nil) *result = o;
    else *result = o->cons.car;
  } else {
    if (o == object_nil) return ip_throw(IndexError, index_out_of_range);
    *result = argv->cons.cdr->cons.car;
    o->cons.car = *result;
  }
  return TRUE;
}

static int string_access(int argc, object argv, object o, object *result)
{
  int i;
  if (!bi_cpint(argv->cons.car, &i)) return FALSE;
  if (argc == 2) return str_slice(o, i, i + 1, result);
  return ip_throw(ArgumentError, too_many_arguments);
}

static int bytes_access(int argc, object argv, object o, object *result)
{
  int i, byte;
  if (!bi_cpint(argv->cons.car, &i)) return FALSE;
  if (!bi_range(0, i, o->mem.size - 1)) return FALSE;
  if (argc == 2) *result = sint(LC(o->mem.elt + i));
  else {
    if (!bi_cbyte((*result = argv->cons.cdr->cons.car), &byte)) return FALSE;
    SC(o->mem.elt + i, byte);
  }
  return TRUE;
}

static int array_access(int argc, object argv, object o, object *result)
{
  int i;
  if (!bi_cpint(argv->cons.car, &i)) return FALSE;
  if (!bi_range(0, i, o->array.size - 1)) return FALSE;
  if (argc == 2) *result = o->array.elt[i];
  else *result = o->array.elt[i] = argv->cons.cdr->cons.car;
  return TRUE;
}

static int dict_access(int argc, object argv, object o, object *result)
{
  object key;
  key = argv->cons.car;
  if (argc == 2) {
    if ((*result = map_get(o, key)) == NULL) *result = object_nil;
  } else {
    *result = argv->cons.cdr->cons.car;
    map_put(o, key, *result);
  }
  return TRUE;
}

DEFUN(_5b__5d_)
{
  object o;
  if (!bi_argc_range(argc, 2, 3)) return FALSE;
  if (!bi_argv(BI_LIST | BI_BYTES | BI_STR | BI_ARRAY | BI_DICT, argv->cons.car, &o)) return FALSE;
  switch (object_type(o)) {
    case SYMBOL:
    case CONS:
      return cons_access(argc, argv->cons.cdr, o, result);
    case STRING:
      return string_access(argc, argv->cons.cdr, o, result);
    case BYTES:
      return bytes_access(argc, argv->cons.cdr, o, result);
    case ARRAY:
      return array_access(argc, argv->cons.cdr, o, result);
    case DICT:
      return dict_access(argc, argv->cons.cdr, o, result);
    default:
      xassert(FALSE);
      return FALSE;
  }
}

int cons_in_p(object o, object p, object *result)
{
  for (*result = object_true; o != object_nil; o = o->cons.cdr)
    if (object_eq_p(o->cons.car, p)) return TRUE;
  *result = object_nil;
  return TRUE;
}

int bytes_in_p(object o, object p, object *result)
{
  int i, byte;
  if (!bi_cbyte(p, &byte)) return FALSE;
  for (i = 0; i < o->mem.size; i++) {
    if (LC(o->mem.elt + i) == byte) {
      *result = object_true;
      return TRUE;
    }
  }
  *result = object_nil;
  return TRUE;
}

int string_in_p(object o, object p, object *result)
{
  int i;
  if (!bi_argv(BI_STR, p, &p)) return FALSE;
  i = 0;
  while (i < o->mem.size && (o->mem.size - i >= p->mem.size)) {
    if (memcmp(o->mem.elt + i, p->mem.elt, p->mem.size) == 0) {
      *result = object_true;
      return TRUE;
    }
    if (!ch_len(LC(o->mem.elt + i), &i)) return FALSE;
  }
  *result = object_nil;
  return TRUE;
}

int array_in_p(object o, object p, object *result)
{
  int i;
  for (i = 0; i < o->array.size; i++) {
    if (object_eq_p(o->array.elt[i], p)) {
      *result = object_true;
      return TRUE;
    }
  }
  *result = object_nil;
  return TRUE;
}

int dict_in_p(object o, object p, object *result)
{
  *result = object_bool(map_get(o, p) != NULL);
  return TRUE;
}

DEFUN(in_3f_)
{
  object o;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_argv(BI_LIST | BI_BYTES | BI_STR | BI_ARRAY | BI_DICT, argv->cons.cdr->cons.car, &o)) return FALSE;
  switch (object_type(o)) {
    case SYMBOL:
    case CONS:
      return cons_in_p(o, argv->cons.car, result);
    case BYTES:
      return bytes_in_p(o, argv->cons.car, result);
    case STRING:
      return string_in_p(o, argv->cons.car, result);
    case ARRAY:
      return array_in_p(o, argv->cons.car, result);
    case DICT:
      return dict_in_p(o, argv->cons.car, result);
    default:
      xassert(FALSE);
      return FALSE;
  }
}

int cons_index(object o, object p, int s, int e, object *result)
{
  int i;
  for (i = 0; o != object_nil && i != e; i++, o = o->cons.cdr) {
    if (i < s) continue;
    if (object_eq_p(o->cons.car, p)) {
      *result = gc_new_xint(i);
      return TRUE;
    }
  }
  *result = object_nil;
  return TRUE;
}

int bytes_index(object o, object p, int s, int e, object *result)
{
  int i, byte;
  if (!bi_cbyte(p, &byte)) return FALSE;
  for (i = s; i < o->mem.size && i != e; i++) {
    if (LC(o->mem.elt + i) == byte) {
      *result = gc_new_xint(i);
      return TRUE;
    }
  }
  *result = object_nil;
  return TRUE;
}

int string_index(object o, object p, int s, int e, object *result)
{
  int i, n;
  if (!bi_argv(BI_STR, p, &p)) return FALSE;
  for (i = n = 0; i < o->mem.size && (o->mem.size - i >= p->mem.size) && n != e; n++) {
    if (n >= s && (memcmp(o->mem.elt + i, p->mem.elt, p->mem.size) == 0)) {
      *result = gc_new_xint(n);
      return TRUE;
    }
    if (!ch_len(LC(o->mem.elt + i), &i)) return FALSE;
  }
  *result = object_nil;
  return TRUE;
}

int array_index(object o, object p, int s, int e, object *result)
{
  int i;
  for (i = s; i < o->array.size && i != e; i++) {
    if (object_eq_p(o->array.elt[i], p)) {
      *result = gc_new_xint(i);
      return TRUE;
    }
  }
  *result = object_nil;
  return TRUE;
}

DEFUN(index)
{
  int s, e;
  object o, p;
  if (!bi_argc_range(argc, 2, 4)) return FALSE;
  p = argv->cons.car;
  if (!bi_argv(BI_LIST | BI_BYTES | BI_STR | BI_ARRAY, (argv = argv->cons.cdr)->cons.car, &o)) return FALSE;
  if (argc < 3) s = 0;
  else if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &s)) return FALSE;
  if (argc < 4) e = -1;
  else if (!bi_cpint(argv->cons.cdr->cons.car, &e)) return FALSE;
  switch (object_type(o)) {
    case SYMBOL:
    case CONS:
      return cons_index(o, p, s, e, result);
    case BYTES:
      return bytes_index(o, p, s, e, result);
    case STRING:
      return string_index(o, p, s, e, result);
    case ARRAY:
      return array_index(o, p, s, e, result);
    default:
      xassert(FALSE);
      return FALSE;
  }
}

int cons_rindex(object o, object p, int s, int e, object *result)
{
  int i, ri;
  for (i = 0, ri = -1; o != object_nil && i != e; i++, o = o->cons.cdr) {
    if (i < s) continue;
    if (object_eq_p(o->cons.car, p)) ri = i;
  }
  if (ri == -1) *result = object_nil;
  else *result = gc_new_xint(ri);
  return TRUE;
}

int bytes_rindex(object o, object p, int s, int e, object *result)
{
  int i, byte;
  if (!bi_cbyte(p, &byte)) return FALSE;
  if (e == -1) e = o->array.size;
  for (i = e - 1; i < o->mem.size && i >= s; i--) {
    if (LC(o->mem.elt + i) == byte) {
      *result = gc_new_xint(i);
      return TRUE;
    }
  }
  *result = object_nil;
  return TRUE;
}

int string_rindex(object o, object p, int s, int e, object *result)
{
  int i, ri, n;
  if (!bi_argv(BI_STR, p, &p)) return FALSE;
  for (i = n = 0, ri = -1; i < o->mem.size && (o->mem.size - i >= p->mem.size) && n != e; n++) {
    if (n >= s && memcmp(o->mem.elt + i, p->mem.elt, p->mem.size) == 0) ri = i;
    if (!ch_len(LC(o->mem.elt + i), &i)) return FALSE;
  }
  if (ri == -1) *result = object_nil;
  else *result = gc_new_xint(ri);
  return TRUE;
}

int array_rindex(object o, object p, int s, int e, object *result)
{
  int i;
  if (e == -1) e = o->array.size;
  for (i = e - 1; i < o->array.size && i >= s; i--) {
    if (object_eq_p(o->array.elt[i], p)) {
      *result = gc_new_xint(i);
      return TRUE;
    }
  }
  *result = object_nil;
  return TRUE;
}

DEFUN(last_2d_index)
{
  int s, e;
  object o, p;
  if (!bi_argc_range(argc, 2, 4)) return FALSE;
  p = argv->cons.car;
  if (!bi_argv(BI_LIST | BI_BYTES | BI_STR | BI_ARRAY, (argv = argv->cons.cdr)->cons.car, &o)) return FALSE;
  if (argc < 3) s = 0;
  else if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &s)) return FALSE;
  if (argc < 4) e = -1;
  else if (!bi_cpint(argv->cons.cdr->cons.car, &e)) return FALSE;
  switch (object_type(o)) {
    case SYMBOL:
    case CONS:
      return cons_rindex(o, p, s, e, result);
    case BYTES:
      return bytes_rindex(o, p, s, e, result);
    case STRING:
      return string_rindex(o, p, s, e, result);
    case ARRAY:
      return array_rindex(o, p, s, e, result);
    default:
      xassert(FALSE);
      return FALSE;
  }
}

DEFUN(len)
{
  int len;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_LIST | BI_BYTES | BI_STR | BI_ARRAY | BI_DICT, argv->cons.car, &o)) return FALSE;
  switch (object_type(o)) {
    case SYMBOL: len = 0; break;
    case CONS: len = list_len(o); break;
    case BYTES: len = o->mem.size; break;
    case STRING: if (!str_len(o, &len)) return FALSE; break;
    case ARRAY: len = o->array.size; break;
    case DICT: len = o->map.entry_count; break;
    default: xassert(FALSE); return FALSE;
  }
  *result = gc_new_xint(len);
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
  if (bi_cint64(argv->cons.car, &i)) {
    if (x >= (double)i) return TRUE;
    return double_lt((double)i, argv->cons.cdr, result);
  }
  if (bi_cdouble(argv->cons.car, &d)) {
    if (x >= d) return TRUE;
    return double_lt(d, argv->cons.cdr, result);
  }
  return ip_throw(ArgumentError, expected_number);
}

static int int64_lt(int64_t x, object argv, object *result)
{
  int64_t y;
  if (argv == object_nil) {
    *result = object_true;
    return TRUE;
  }
  if (bi_cint64(argv->cons.car, &y)) {
    if (x >= y) return TRUE;
    return int64_lt(y, argv->cons.cdr, result);
  }
  return double_lt((double)x, argv, result);
}

static int num_lt(object o, object argv, object *result)
{
  int64_t i;
  double d;
  if (bi_cint64(o, &i)) return int64_lt(i, argv, result);
  if (bi_cdouble(o, &d)) return double_lt(d, argv, result);
  xassert(FALSE);
  return FALSE;
}

static int bytes_lt(object o, object argv, object *result)
{
  int i, x;
  object p;
  if (argv == object_nil) {
    *result = object_true;
    return TRUE;
  }
  if (!bi_argv(BI_BYTES | BI_STR | BI_SYM | BI_KEY, argv->cons.car, &p)) return FALSE;
  for (i = 0; i < p->mem.size; i++) {
    if (i == o->mem.size) x = -1;
    else x = LC(o->mem.elt + i) - LC(p->mem.elt + i);
    if (x < 0) return bytes_lt(p, argv->cons.cdr, result);
    if (x > 0) break;
  }
  return  TRUE;
}

static int chcmp(object o, int *oi, object p, int *pi, int *x)
{
  int olen, plen, oord, pord;
  olen = plen = 0;
  if (!ch_len(LC(o->mem.elt + *oi), &olen)) return FALSE;
  if (!ch_len(LC(p->mem.elt + *pi), &plen)) return FALSE;
  if (!ord(o, *oi, olen, &oord)) return FALSE;
  if (!ord(p, *pi, plen, &pord)) return FALSE;
  *oi += olen;
  *pi += plen;
  *x = oord - pord;
  return TRUE;
}

static int str_lt(object o, object argv, object *result)
{
  int oi, pi, x;
  object p;
  if (argv == object_nil) {
    *result = object_true;
    return TRUE;
  }
  if (!bi_argv(BI_STR, argv->cons.car, &p)) return FALSE;
  oi = pi = 0;
  while (pi < p->mem.size) {
    if (oi == o->mem.size) x = -1;
    else if (!chcmp(o, &oi, p, &pi, &x)) return FALSE;
    if (x < 0) return str_lt(p, argv->cons.cdr, result);
    if (x > 0) break;
  }
  return  TRUE;
}

DEFUN(_3c_)
{
  object o;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_argv(BI_NUM | BI_SYM | BI_KEY | BI_BYTES | BI_STR, argv->cons.car, &o)) return FALSE;
  *result = object_nil;
  switch (object_type(o)) {
    case SINT:
    case XINT:
    case XFLOAT:
      return num_lt(o, argv->cons.cdr, result);
    case SYMBOL:
    case KEYWORD:
    case BYTES:
      return bytes_lt(o, argv->cons.cdr, result);
    case STRING:
      return str_lt(o, argv->cons.cdr, result);
    default:
      xassert(FALSE);
      return FALSE;
  }
}

// os.

DEFUN(fp)
{
  int fd;
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cint(argv->cons.car, &fd)) return FALSE;
  switch (fd) {
    case 0: fp = stdin; break;
    case 1: fp = stdout; break;
    case 2: fp = stderr; break;
    default: return ip_throw(OSError, fp_failed);
  }
  *result = gc_new_xint((intptr_t)fp);
  return TRUE;
}

static char *fopen_mode_table[] = {
  "rb",
  "wb",
  "ab",
  "rb+"
};

DEFUN(fopen)
{
  char *fn;
  int mode;
  FILE *fp;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cstring(argv, &fn)) return FALSE;
  if (!bi_cpint(argv->cons.cdr->cons.car, &mode)) return FALSE;
  if (mode >= sizeof(fopen_mode_table) / sizeof(char *))
    return ip_throw(ArgumentError, invalid_args);
  if ((fp = pf_fopen(fn, fopen_mode_table[mode])) == NULL)
    return ip_throw(OSError, fopen_failed);
  *result = gc_new_xint((intptr_t)fp);
  return TRUE;
}

DEFUN(fgetc)
{
  int ch;
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  ch = fgetc(fp);
  if (ch == EOF && ferror(fp)) {
    clearerr(fp);
    return ip_throw(OSError, fgetc_failed);
  }
  *result = gc_new_xint(ch);
  return TRUE;
}

DEFUN(fputc)
{
  int byte;
  FILE *fp;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  *result = argv->cons.car;
  if (!bi_cbyte(argv->cons.car, &byte)) return FALSE;
  if (!bi_cintptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  if (fputc(byte, fp) == EOF)
    return ip_throw(OSError, fputc_failed);
  return TRUE;
}

DEFUN(fgets)
{
  char *s;
  FILE *fp;
  struct xbarray x;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  xbarray_init(&x);
  s = xbarray_fgets(&x, fp);
  if (s == NULL) *result = object_nil;
  else {
    *result = gc_new_mem(STRING, --x.size);    // remove last NUL
    memcpy((*result)->mem.elt, x.elt, x.size);
  }
  xbarray_free(&x);
  return TRUE;
}

DEFUN(fread)
{
  int from, size;
  FILE *fp;
  object o;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_argv(BI_BYTES, argv->cons.car, &o)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_cint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!bi_range(0, from + size, o->mem.size)) return FALSE;
  if (!bi_cintptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  size = fread(o->mem.elt + from, 1, size, fp);
  if (size == 0 && ferror(fp)) {
    clearerr(fp);
    return ip_throw(OSError, fread_failed);
  }
  *result = gc_new_xint(size);
  return TRUE;
}

DEFUN(fwrite)
{
  int from, size;
  FILE *fp;
  object o;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_argv(BI_BYTES | BI_STR | BI_SYM | BI_KEY, argv->cons.car, &o)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!bi_range(0, from + size, o->mem.size)) return FALSE;
  if (!bi_cintptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  size = fwrite(o->mem.elt + from, 1, size, fp);
  if (size == 0 && ferror(fp)) {
    clearerr(fp);
    return ip_throw(OSError, fwrite_failed);
  }
  *result = gc_new_xint(size);
  return TRUE;
}

DEFUN(fseek)
{
  int st, off;
  FILE *fp;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  if (!bi_cint(argv->cons.cdr->cons.car, &off)) return FALSE;
  *result = object_nil;
  if (off == -1) st = fseek(fp, 0, SEEK_END);
  else st = fseek(fp, off, SEEK_SET);
  if (st != 0) return ip_throw(OSError, fseek_failed);
  return TRUE;
}

DEFUN(ftell)
{
  int pos;
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  if ((pos = ftell(fp)) == -1) return ip_throw(OSError, ftell_failed);
  *result = gc_new_xint(pos);
  return TRUE;
}

DEFUN(fclose)
{
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  fclose(fp);
  *result = object_nil;
  return TRUE;
}

DEFUN(stat)
{
  int mode;
  char *fn;
  struct pf_stat statbuf;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &fn)) return FALSE;
  mode = pf_stat(fn, &statbuf);
  if (mode == PF_ERROR) return ip_throw(OSError, stat_failed);
  if (mode == PF_NONE) {
    *result = object_nil;
    return TRUE;
  }
  *result = gc_new_array(3);
  (*result)->array.elt[0] = gc_new_xint(mode);
  (*result)->array.elt[1] = gc_new_xint(statbuf.size);
  (*result)->array.elt[2] = gc_new_xint(statbuf.mtime);
  return TRUE;
}

DEFUN(utime)
{
  char *fn;
  int64_t tv;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cstring(argv, &fn)) return FALSE;
  if (!bi_cint64(argv->cons.cdr->cons.car, &tv)) return ip_throw(ArgumentError, expected_integer);
  if (!pf_utime(fn, tv)) return ip_throw(OSError, error_nil);
  *result = object_nil;
  return TRUE;
}

DEFUN(getcwd)
{
  char buf[MAX_STR_LEN];
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  pf_getcwd(buf);
  *result = gc_new_mem_from(STRING, buf, strlen(buf));
  return TRUE;
}

DEFUN(chdir)
{
  char *fn;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &fn)) return FALSE;
  *result = object_nil;
  return pf_chdir(fn);
}

DEFUN(readdir)
{
  char *path;
  struct xbarray dirs;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &path)) return FALSE;
  xbarray_init(&dirs);
  if (!pf_readdir(path, &dirs))
    return ip_throw(OSError, readdir_failed);
  *result = gc_new_mem_from(STRING, dirs.elt, dirs.size);
  xbarray_free(&dirs);
  return TRUE;
}

DEFUN(remove)
{
  char *fn;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &fn)) return FALSE;
  *result = object_nil;
  return pf_remove(fn);
}

DEFUN(mkdir)
{
  char *path;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &path)) return FALSE;
  *result = object_nil;
  return pf_mkdir(path);
}

DEFUN(rename)
{
  char *src_dst[2];
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cstrings(2, argv, src_dst)) return FALSE;
  *result = object_nil;
  return rename(src_dst[0], src_dst[1]) == 0;
}

DEFUN(time)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  *result = gc_new_xint(time(NULL));
  return TRUE;
}

DEFUN(clock)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  *result = gc_new_xfloat((double)clock() / CLOCKS_PER_SEC);
  return TRUE;
}

DEFUN(sleep)
{
  double t;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cdouble(argv->cons.car, &t)) return FALSE;
  xsleep(t);
  *result = object_nil;
  return TRUE;
}

static int daysec(struct tm *t)
{
  return (t->tm_hour * 60 + t->tm_min) * 60 + t->tm_sec;
}

DEFUN(utcoffset)
{
  int off;
  time_t now;
  struct tm gtm, ltm;
  now = time(NULL);
  gtm = *gmtime(&now);
  ltm = *localtime(&now);
  if (gtm.tm_yday == ltm.tm_yday) off = 0;
  else if (gtm.tm_yday == ltm.tm_yday + 1) off = -1;
  else off = 1;
  *result = gc_new_xint(daysec(&ltm) + off * 24 * 60 * 60 - daysec(&gtm));
  return TRUE;
}

static char *popen_mode_table[] = {
  "r",
  "w"
};

DEFUN(popen)
{
  char *s;
  int mode;
  FILE *fp;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cstring(argv, &s)) return FALSE;
  if (!bi_cpint(argv->cons.cdr->cons.car, &mode)) return FALSE;
  if (mode >= sizeof(popen_mode_table) / sizeof(char *)) return ip_throw(ArgumentError, invalid_args);
  if ((fp = popen(s, popen_mode_table[mode])) == NULL) return ip_throw(OSError, fopen_failed);
  *result = gc_new_xint((intptr_t)fp);
  return TRUE;
}

DEFUN(pclose)
{
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  pclose(fp);
  *result = object_nil;
  return TRUE;
}

DEFUN(system)
{
  char *s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &s)) return FALSE;
  *result = gc_new_xint(system(s));
  return TRUE;
}

DEFUN(getenv)
{
  char *s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &s)) return FALSE;
  if ((s = getenv(s)) == NULL) *result = object_nil;
  else *result = gc_new_mem_from(STRING, s, strlen(s));
  return TRUE;
}

DEFUN(putenv)
{
  char *kv[2];
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cstrings(2, argv, kv)) return FALSE;
  *strchr(kv[0], '\0') = '=';
  *result = object_nil;
  return putenv(xstrdup(kv[0])) == 0;
}

#undef DEFSP
#undef DEFUN

#define DEFSP(name) extern int special_##name(int, object);
#include "defsp.wk"
#undef DEFSP

#define DEFUN(name) extern int function_##name(int, object, object *);
#include "defun.wk"
#undef DEFUN

char *special_name_table[] = {
#define DEFSP(name) #name,
#include "defsp.wk"
#undef DEFSP
  NULL
};

char *function_name_table[] = {
#define DEFUN(name) #name,
#include "defun.wk"
#undef DEFUN
  NULL
};

int (*special_table[])(int argc, object argv) = {
#define DEFSP(name) special_##name,
#include "defsp.wk"
#undef DEFSP
  NULL
};

int (*function_table[])(int argc, object argv, object *result) = {
#define DEFUN(name) function_##name,
#include "defun.wk"
#undef DEFUN
  NULL
};

static int digit_val(char ch)
{
  if (isdigit(ch)) return ch - '0';
  return ch - 'a' + 10;
}

char *bi_as_symbol_name(char *name, char *buf)
{
  char len;
  int s, t;
  if (name == NULL) return NULL;
  s = t = 0;
  len = strlen(name);
  while (s < len) {
    if (name[s] != '_') buf[t++] = name[s++];
    else {
      buf[t++] = 16 * digit_val(name[s + 1]) + digit_val(name[s + 2]);
      s += 4;
    }
  }
  buf[t] = '\0';
  return buf;
}
