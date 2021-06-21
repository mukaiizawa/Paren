// built-in.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
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

int bi_range(int min, int x, int max)
{
  if (x < min || x > max)
    return ip_throw(IndexError, index_out_of_range);
  return TRUE;
}

int bi_cons(object o, object *result)
{
  if (object_type(*result = o) != CONS)
    return ip_throw(ArgumentError, expected_cons);
  return TRUE;
}

int bi_string(object o, object *result)
{
  if (object_type(*result = o) != STRING)
    return ip_throw(ArgumentError, expected_string);
  return TRUE;
}

int bi_symbol(object o, object *result)
{
  if (object_type(*result = o) != SYMBOL)
    return ip_throw(ArgumentError, expected_symbol);
  return TRUE;
}

int bi_keyword(object o, object *result)
{
  if (object_type(*result = o) != KEYWORD)
    return ip_throw(ArgumentError, expected_keyword);
  return TRUE;
}

int bi_bytes(object o, object *result)
{
  if (object_type(*result = o) != BYTES)
    return ip_throw(ArgumentError, expected_bytes);
  return TRUE;
}

int bi_array(object o, object *result)
{
  if (object_type(*result = o) != ARRAY)
    return ip_throw(ArgumentError, expected_array);
  return TRUE;
}

int bi_dict(object o, object *result)
{
  if (object_type(*result = o) != DICT)
    return ip_throw(ArgumentError, expected_dict);
  return TRUE;
}

int bi_func(object o, object *result)
{
  if (object_type(*result = o) != FUNC)
    return ip_throw(ArgumentError, expected_function);
  return TRUE;
}

int bi_list(object o, object *result)
{
  *result = o;
  if (!list_p(o))
    return ip_throw(ArgumentError, expected_list);
  return TRUE;
}

int bi_bytes_like(object o, object *result)
{
  if (!bytes_like_p(*result = o))
    return ip_throw(ArgumentError, expected_bytes_like);
  return TRUE;
}

int bi_sequence(object o, object *result)
{
  if (!sequence_p(*result = o))
    return ip_throw(ArgumentError, expected_sequence);
  return TRUE;
}

int bi_mutable_sequence(object o, object *result)
{
  if (!mutable_sequence_p(*result = o))
    return ip_throw(ArgumentError, expected_mutable_sequence);
  return TRUE;
}

int bi_collection(object o, object *result)
{
  if (!collection_p(*result = o))
    return ip_throw(ArgumentError, expected_collection);
  return TRUE;
}

int bi_symbol_keyword(object o, object *result)
{
  if (!symbol_keyword_p(*result = o))
    return ip_throw(ArgumentError, expected_symbol_keyword);
  return TRUE;
}

int bi_builtin(object o, object *result)
{
  if (!builtin_p(*result = o))
    return ip_throw(ArgumentError, expected_builtin_operator);
  return TRUE;
}

int bi_proc(object o, object *result)
{
  switch (object_type(*result = o)) {
    case FUNC:
    case MACRO:
      return TRUE;
    default:
      return ip_throw(ArgumentError, expected_function_macro);
  }
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
  int i;
  int offset[MAX_STRINGS]; // xbarray use realloc.
  object o;
  xassert(n <= MAX_STRINGS);
  xassert(object_type(argv) == CONS);
  xbarray_reset(&bi_buf);
  for (i = 0; i < n; i++) {
    if (!bi_string(argv->cons.car, &o)) return FALSE;
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
    case BUILTINFUNC:
    case FUNC:
      *result = object_bool(TRUE);
      break;
    default:
      *result = object_bool(FALSE);
      break;
  }
  return TRUE;
}

DEFUN(builtin_3f_)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(builtin_p(argv->cons.car));
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

DEFUN(builtin_2d_name)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_builtin(argv->cons.car, &o)) return FALSE;
  *result = o->builtin.name;
  return TRUE;
}

DEFUN(procparams)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_proc(argv->cons.car, &o)) return FALSE;
  *result = o->proc.params;
  return TRUE;
}

DEFUN(procbody)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_proc(argv->cons.car, &o)) return FALSE;
  *result = o->proc.body;
  return TRUE;
}

// list.

DEFUN(cons)
{
  object o;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_list(argv->cons.cdr->cons.car, &o)) return FALSE;
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
  if (!bi_list(argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.car;
  return TRUE;
}

DEFUN(car_21_)
{
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cons(argv->cons.car, &o)) return FALSE;
  p = argv->cons.cdr->cons.car;
  o->cons.car = p;
  *result = p;
  return TRUE;
}

DEFUN(cdr)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_list(argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.cdr;
  return TRUE;
}

DEFUN(cdr_21_)
{
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cons(argv->cons.car, &o)) return FALSE;
  if (!bi_list(argv->cons.cdr->cons.car, &p)) return FALSE;
  o->cons.cdr = p;
  *result = p;
  return TRUE;
}

DEFUN(last_2d_cons)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_list(argv->cons.car, &o)) return FALSE;
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
  if (!bi_list(argv->cons.car, &o)) return FALSE;
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
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
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

DEFUN(_3c_)
{
  int64_t i;
  double d;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  *result = object_nil;
  if (bi_cint64(argv->cons.car, &i))
    return int64_lt(i, argv->cons.cdr, result);
  if (bi_cdouble(argv->cons.car, &d))
    return double_lt(d, argv->cons.cdr, result);
  return ip_throw(ArgumentError, expected_number);
}

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

// bytes & bytes-like.

static int bytes_like_to(int type, int argc, object argv, object *result)
{
  int i, size;
  object o;
  if (!bi_argc_range(argc, 1, 3)) return FALSE;
  if (!bi_bytes_like(argv->cons.car, &o)) return FALSE;
  if (argc < 2) i = 0;
  else if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (argc < 3) size = o->mem.size - i;
  else if (!bi_cint(argv->cons.cdr->cons.car, &size)) return FALSE;
  if (!bi_range(0, i + size, o->mem.size)) return FALSE;
  *result = gc_new_mem_from(type, o->mem.elt + i, size);
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
  if (!bi_bytes(argv->cons.car, &o)) return FALSE;
  object_reset_type(o, STRING);
  *result = o;
  return TRUE;
}

DEFUN(memcpy)
{
  int oi, pi, size;
  object o, p;
  if (!bi_argc_range(argc, 5, 5)) return FALSE;
  if (!bi_bytes_like(argv->cons.car, &o)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &oi)) return FALSE;
  if (!bi_bytes((argv = argv->cons.cdr)->cons.car, &p)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &pi)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!bi_range(0, oi + size, o->mem.size)) return FALSE;
  if (!bi_range(0, pi + size, p->mem.size)) return FALSE;
  memmove(p->mem.elt + pi, o->mem.elt + oi, size);
  *result = p;
  return TRUE;
}

DEFUN(memmem)
{
  int b, s, e;
  object o, p;
  if (!bi_argc_range(argc, 2, 4)) return FALSE;
  if (!bi_bytes_like(argv->cons.car, &o)) return FALSE;
  b = 0;
  p = NULL;
  argv = argv->cons.cdr;
  switch (object_type(argv->cons.car)) {
    case SINT:
      if (!bi_cbyte(argv->cons.car, &b)) return FALSE;
      break;
    case SYMBOL:
    case KEYWORD:
    case BYTES:
    case STRING:
      p = argv->cons.car;
      if (o->mem.size < p->mem.size) {
        *result = object_nil;
        return TRUE;
      }
      break;
    default:
      return FALSE;
  }
  if (argc < 3) s = 0;
  else if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &s)) return FALSE;
  if (argc < 4) e = o->mem.size;
  else if (!bi_cpint(argv->cons.cdr->cons.car, &e)) return FALSE;
  if (!bi_range(0, s, e)) return FALSE;
  if (!bi_range(0, e, o->mem.size)) return FALSE;
  if (p == NULL) {
    while (s < e) {
      if (LC(o->mem.elt + s) == b) {
        *result = sint(s);
        return TRUE;
      }
      s++;
    }
  } else {
    while (s < e) {
      if (s + p->mem.size > o->mem.size) break;
      if (memcmp(o->mem.elt + s, p->mem.elt, p->mem.size) == 0) {
        *result = sint(s);
        return TRUE;
      }
      s++;
    }
  }
  *result = object_nil;
  return TRUE;
}

DEFUN(memlen)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_bytes_like(argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint(o->mem.size);
  return TRUE;
}

DEFUN(memcmp)
{
  int val, size;
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_bytes_like(argv->cons.car, &o)) return FALSE;
  if (!bi_bytes_like(argv->cons.cdr->cons.car, &p)) return FALSE;
  size = o->mem.size;
  if (size > p->mem.size) size = p->mem.size;
  if ((val = memcmp(o->mem.elt, p->mem.elt, size)) == 0) {
    if (o->mem.size < p->mem.size) val = -1;
    else if (o->mem.size > p->mem.size) val = 1;
  }
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
    if (!bi_bytes_like(argv->cons.car, &p)) {
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

DEFUN(memcat)
{
  object o;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  if (!bi_bytes_like(argv->cons.car, &o)) return FALSE;
  return bytes_like_concat(o, argv->cons.cdr, result);
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

DEFUN(ord)
{
  int x;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_string(argv->cons.car, &o)) return FALSE;
  switch (o->mem.size) {
    case 1:
      x = LC(o->mem.elt);
      break;
    case 2:
      x = ((LC(o->mem.elt) & 0x3f) << 6)
        | (LC(o->mem.elt + 1) & 0x3f);
      break;
    case 3:
      x = ((LC(o->mem.elt) & 0xf) << 12)
        | ((LC(o->mem.elt + 1) & 0x3f) << 6)
        | (LC(o->mem.elt + 2) & 0x3f);
      break;
    case 4:
      x = ((LC(o->mem.elt) & 0x3) << 18)
        | ((LC(o->mem.elt + 1) & 0x3f) << 12)
        | ((LC(o->mem.elt + 2) & 0x3f) << 6)
        | (LC(o->mem.elt + 3) & 0x3f);
      break;
    default:
      return ip_throw(ArgumentError, invalid_utf8_byte_sequence);
  }
  *result = gc_new_xint(x);
  return TRUE;
}

DEFUN(ascii_3f_)
{
  int len;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_string(argv->cons.car, &o)) return FALSE;
  if (!str_len(o, &len)) return FALSE;
  *result = object_bool(len == o->mem.size);
  return TRUE;
}

static int xctype_p(int argc, object argv, int (*f)(int c), object *result)
{
  int i;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_string(argv->cons.car, &o)) return FALSE;
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
  if (!bi_string(argv->cons.car, &o)) return FALSE;
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
  int i, j, w, size;
  if (!str_len(o, &size)) return FALSE;
  *result = gc_new_array(size);
  i = j = w = 0;
  while (i < size) {
    if (!ch_len(LC(o->mem.elt + j), &w)) return FALSE;
    (*result)->array.elt[i] = gc_new_mem_from(STRING, o->mem.elt + j, w);
    i++;
    j += w;
    w = 0;
  }
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
  if (!bi_dict(argv->cons.car, &o)) return FALSE;
  *result = map_keys(o);
  return TRUE;
}

DEFUN(_7b__7d_)
{
  object o, key;
  if (!bi_argc_range(argc, 2, 3)) return FALSE;
  if (!bi_dict(argv->cons.car, &o)) return FALSE;
  if (!bi_symbol_keyword((argv = argv->cons.cdr)->cons.car, &key)) return FALSE;
  if (argc == 2) {
    if ((*result = map_get(o, key)) == NULL) *result = object_nil;
  } else {
    *result = argv->cons.cdr->cons.car;
    map_put(o, key, *result);
  }
  return TRUE;
}

// sequence.

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
  }
  *result = gc_new_mem_from(STRING, o->mem.elt + s, t - s);
  return TRUE;
}

DEFUN(slice)
{
  int start, stop;
  object o;
  if (!bi_argc_range(argc, 1, 3)) return FALSE;
  if (!bi_sequence(argv->cons.car, &o)) return FALSE;
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
      else if (!bi_range(0, stop, o->mem.size)) return FALSE;
      *result = gc_new_mem_from(BYTES, o->mem.elt + start, stop - start);
      return TRUE;
    case STRING:
      return str_slice(o, start, stop, result);
    case ARRAY:
      if (stop == -1) stop = o->array.size;
      else if (!bi_range(0, stop, o->array.size)) return FALSE;
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
    if (!bi_list(argv->cons.car, &o)) return FALSE;
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
    if (!bi_array(p->cons.car, &q)) return FALSE;
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
  if (!bi_sequence(argv->cons.car, &o)) return FALSE;
  switch (object_type(o)) {
    case SYMBOL:
    case CONS:
      return cons_concat(argv, result);
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

DEFUN(_5b__5d_)
{
  int i, byte;
  object o;
  if (!bi_argc_range(argc, 2, 3)) return FALSE;
  if (!bi_sequence(argv->cons.car, &o)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  switch (object_type(o)) {
    case SYMBOL:
    case CONS:
      while (o != object_nil) {
        if (i-- == 0) break;
        o = o->cons.cdr;
      }
      if (argc == 2) {
        if (o == object_nil) *result = o;
        else *result = o->cons.car;
      } else {
        if (o == object_nil)
          return ip_throw(IndexError, index_out_of_range);
        *result = argv->cons.cdr->cons.car;
        o->cons.car = *result;
      }
      return TRUE;
    case STRING:
      if (argc == 2) return str_slice(o, i, i + 1, result);
      return ip_throw(ArgumentError, expected_mutable_sequence);
    case BYTES:
      if (!bi_range(0, i, o->mem.size - 1)) return FALSE;
      if (argc == 2) *result = sint(LC(o->mem.elt + i));
      else {
        if (!bi_cbyte((*result = argv->cons.cdr->cons.car), &byte)) return FALSE;
        SC(o->mem.elt + i, byte);
      }
      return TRUE;
    case ARRAY:
      if (!bi_range(0, i, o->array.size - 1)) return FALSE;
      if (argc == 2) *result = o->array.elt[i];
      else *result = o->array.elt[i] = argv->cons.cdr->cons.car;
      return TRUE;
    default:
      xassert(FALSE);
      return FALSE;
  }
}

// collection.

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
  if (!bi_string(p, &p)) return FALSE;
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
  if (!bi_symbol_keyword(p, &p)) return FALSE;
  *result = object_bool(map_get(o, p) != NULL);
  return TRUE;
}

DEFUN(in_3f_)
{
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_collection(argv->cons.cdr->cons.car, &o)) return FALSE;
  switch (object_type(o)) {
    case SYMBOL:
      *result = object_nil;
      return TRUE;
    case CONS:
      p = argv->cons.car;
      while (!object_eq_p(o->cons.car, p)) {
        if ((o = o->cons.cdr) == object_nil) {
          *result = object_nil;
          return TRUE;
        }
      }
      *result = object_true;
      return TRUE;
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

static int map_len(object o)
{
  int i, len;
  object *table;
  xassert(object_type(o) == DICT);
  table = o->map.table;
  for (i = len = 0; i < o->map.half_size; i++)
    if (table[i] != NULL) len++;
  return len;
}

DEFUN(len)
{
  int len;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_collection(argv->cons.car, &o)) return FALSE;
  switch (object_type(o)) {
    case SYMBOL:
      len = 0;
      break;
    case CONS:
      len = list_len(o);
      break;
    case BYTES:
      len = o->mem.size;
      break;
    case STRING:
      if (!str_len(o, &len)) return FALSE;
      break;
    case ARRAY:
      len = o->array.size;
      break;
    case DICT:
      len = map_len(o);
    default:
      xassert(FALSE);
      return FALSE;
  }
  *result = gc_new_xint(len);
  return TRUE;
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
