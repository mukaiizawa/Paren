// built-in.

#include "std.h"

#include <math.h>

#include "xsplay.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

// basic functions

PRIM(samep)
{
  int b;
  object o;
  if (argc < 2) return FALSE;
  o = argv->cons.car;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!(b = (o == argv->cons.car))) break;
  }
  *result = object_bool(b);
  return TRUE;
}

static int equal_s_expr_p(object o, object p);
static int equal_cons_p(object o, object p)
{
  if (!equal_s_expr_p(o->cons.car, p->cons.car)) return FALSE;
  return equal_s_expr_p(o->cons.cdr, p->cons.cdr);
}

static int equal_barray(object o, object p)
{
  int size;
  if ((size = o->barray.size) != p->barray.size) return FALSE;
  return memcmp(o->barray.elt, p->barray.elt, size) == 0;
}

static int equal_s_expr_p(object o, object p)
{
  double x, y;
  if (o == p) return TRUE;
  switch (type(o)) {
    case XINT:
    case XFLOAT:
      return bi_double(o, &x) && bi_double(p, &y) && x == y;
    case CONS:
      return typep(p, CONS) && equal_cons_p(o, p);
    case BARRAY:
      return typep(p, BARRAY) && equal_barray(o, p);
    case STRING:
      return typep(p, STRING) && equal_barray(o, p);
    default:
      return FALSE;
  }
}

PRIM(equalp)
{
  int b;
  object o, p;
  if (argc < 2) return FALSE;
  o = argv->cons.car;
  while ((argv = argv->cons.cdr) != object_nil) {
    p = argv->cons.car;
    if (!(b = equal_s_expr_p(o, p))) break;
  }
  *result = object_bool(b);
  return TRUE;
}

PRIM(atom_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(!typep(argv->cons.car, CONS));
  return TRUE;
}

PRIM(number_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(numberp(argv->cons.car));
  return TRUE;
}

PRIM(integer_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, XINT));
  return TRUE;
}

PRIM(symbol_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, SYMBOL));
  return TRUE;
}

PRIM(keyword_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, KEYWORD));
  return TRUE;
}

PRIM(string_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, STRING));
  return TRUE;
}

PRIM(lambda_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, LAMBDA));
  return TRUE;
}

PRIM(macro_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, MACRO));
  return TRUE;
}

PRIM(barray_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, BARRAY));
  return TRUE;
}

PRIM(array_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, ARRAY));
  return TRUE;
}

static int special_p(object o)
{
  int i, size;
  char *s;
  if (!typep(o, SYMBOL)) return FALSE;
  size = o->barray.size;
  for (i = 0; (s = bi_as_symbol_name(special_name_table[i])) != NULL; i++) {
    if (size  == strlen(s) && memcmp(o->barray.elt, s, size) == 0) return TRUE;
  }
  return FALSE;
}

static int prim_p(object o)
{
  int i, size;
  char *s;
  if (!typep(o, SYMBOL)) return FALSE;
  size = o->barray.size;
  for (i = 0; (s = bi_as_symbol_name(prim_name_table[i])) != NULL; i++) {
    if (size  == strlen(s) && memcmp(o->barray.elt, s, size) == 0) return TRUE;
  }
  return FALSE;
}

PRIM(special_operator_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(special_p(argv->cons.car));
  return TRUE;
}

PRIM(operator_p)
{
  object o;
  if (argc != 1) return FALSE;
  o = argv->cons.car;
  *result = object_bool(special_p(o) || prim_p(o) || typep(o, LAMBDA));
  return TRUE;
}

// TODO should be removed
PRIM(print)
{
  char buf[MAX_STR_LEN];
  if (argc != 1) return FALSE;
  *result = argv->cons.car;
  printf("%s\n", object_describe(*result, buf));
  return TRUE;
}

// generic function

static void xbarray_add_barray(struct xbarray *x, object o)
{
  memcpy(xbarray_reserve(x, o->barray.size), o->barray.elt, o->barray.size);
}

static void s_expr_to_string(object o, struct xbarray *x);
static void describe_cons(object o, struct xbarray *x)
{
  s_expr_to_string(o->cons.car, x);
  while ((o = o->cons.cdr) != object_nil) {
    xbarray_add(x, ' ');
    s_expr_to_string(o->cons.car, x);
    if (x->size > MAX_STR_LEN) return;
  }
}

static void describe_array(object o, struct xbarray *x)
{
  int i;
  xbarray_adds(x, "#[");
  for (i = 0; i < o->array.size; i++) {
    if (i != 0) xbarray_add(x, ' ');
    s_expr_to_string(o->array.elt[i], x);
    if (x->size > MAX_STR_LEN) return;
  }
  xbarray_add(x, ']');
}

static void s_expr_to_string(object o, struct xbarray *x)
{
  object p;
  if (x->size > MAX_STR_LEN) return;
  switch (type(o)) {
    case MACRO:
    case LAMBDA:
      if (typep(o, MACRO)) xbarray_adds(x, "(macro ");
      else xbarray_adds(x, "(lambda ");
      if (o->lambda.params == object_nil) xbarray_adds(x, "()");
      else s_expr_to_string(o->lambda.params, x);
      if (o->lambda.body != object_nil) {
        xbarray_add(x, ' ');
        describe_cons(o->lambda.body, x);
      }
      xbarray_add(x, ')');
      break;
    case CONS:
      p = o->cons.car;
      if ((p == object_quote || p == object_not)
          && typep(o->cons.cdr, CONS) && o->cons.cdr->cons.cdr == object_nil)
      {
        if (p == object_quote) xbarray_add(x, '\'');
        if (p == object_not) xbarray_add(x, '!');
        s_expr_to_string(o->cons.cdr->cons.car, x);
      } else {
        xbarray_add(x, '(');
        describe_cons(o, x);
        xbarray_add(x, ')');
      }
      break;
    case XINT:
      xbarray_addf(x, "%d", o->xint.val);
      break;
    case XFLOAT:
      xbarray_addf(x, "%g", o->xfloat.val);
      break;
    case KEYWORD:
      xbarray_add(x, ':');
      xbarray_add_barray(x, o);
      break;
    case SYMBOL:
    case STRING:
    case BARRAY:
      xbarray_add_barray(x, o);
      break;
    case ARRAY:
      describe_array(o, x);
      break;
    default: xassert(FALSE);
  }
}

static object to_string(object o)
{
  struct xbarray x;
  xbarray_init(&x);
  s_expr_to_string(o, &x);
  o = gc_new_barray_from(STRING, x.size, x.elt);
  xbarray_free(&x);
  return o;
}

static int string_add(object argv, object *result)
{
  object x, y;
  if (argv == object_nil) return TRUE;
  x = to_string(*result);
  y = to_string(argv->cons.car);
  *result = gc_new_barray(STRING, x->barray.size + y->barray.size);
  memcpy((*result)->barray.elt, x->barray.elt, x->barray.size);
  memcpy((*result)->barray.elt + x->barray.size, y->barray.elt, y->barray.size);
  return string_add(argv->cons.cdr, result);
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
  return FALSE;
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

static int cons_add(object argv, object *result)
{
  object o;
  o = *result;
  while (o->cons.cdr != object_nil) o = o->cons.cdr;
  o->cons.cdr = argv; // destructive
  return TRUE;
}

PRIM(add)
{
  if (argv == 0) return FALSE;
  *result = argv->cons.car;
  switch (type(*result)) {
    case CONS:
      return cons_add(argv->cons.cdr, result);
    case XINT:
    case XFLOAT:
      return int64_add(argv->cons.cdr, result);
    case STRING:
      return string_add(argv->cons.cdr, result);
    default:
      return FALSE;
  }
}

static int string_length(object o, int *result)
{
  int i;
  unsigned char first;
  i = *result = 0;
  while (i < o->barray.size) {
    if ((first = o->barray.elt[i]) < 0x80) i += 1;
    else if (first < 0xe0) i += 2;
    else if (first < 0xf0) i += 3;
    else i += 4;
    printf("%d\n", first);
    *result += 1;
  }
  return i == o->barray.size;
}

PRIM(length)
{
  int len;
  if (argc != 1) return FALSE;
  if (argv->cons.car == object_nil) len = 0;
  else {
    switch (type(argv->cons.car)) {
      case CONS:
        len = object_list_len(argv->cons.car);
        break;
      case STRING:
        if (!string_length(argv->cons.car, &len)) return FALSE;
        break;
      case BARRAY:
      case ARRAY:
        len = argv->cons.car->array.size;
        break;
      default:
        return FALSE;
    }
  }
  *result = gc_new_xint(len);
  return TRUE;
}

// conversion

PRIM(to_string)
{
  if (argc != 1) return FALSE;
  *result = to_string(argv->cons.car);
  return TRUE;
}

PRIM(to_barray)
{
  object x;
  if (argc != 1) return FALSE;
  switch (type(x = argv->cons.car)) {
    case BARRAY:
      *result = x;
      break;
    case SYMBOL:
    case KEYWORD:
    case STRING:
      *result = gc_new_barray_from(BARRAY, x->barray.size, x->barray.elt);
      break;
    default:
      return FALSE;
  }
  return TRUE;
}

PRIM(to_symbol)
{
  object x;
  if (argc != 1) return FALSE;
  switch (type(x = argv->cons.car)) {
    case SYMBOL:
      *result = x;
      break;
    case KEYWORD:
    case STRING:
      *result = gc_new_barray_from(SYMBOL, x->barray.size, x->barray.elt);
      break;
    default:
      return FALSE;
  }
  return TRUE;
}

PRIM(to_keyword)
{
  object x;
  if (argc != 1) return FALSE;
  switch (type(x = argv->cons.car)) {
    case KEYWORD:
      *result = x;
      break;
    case SYMBOL:
    case STRING:
      *result = gc_new_barray_from(KEYWORD, x->barray.size, x->barray.elt);
      break;
    default:
      return FALSE;
  }
  return TRUE;
}

// list

PRIM(cons)
{
  if (argc != 2 || !listp(argv->cons.cdr->cons.car)) return FALSE;
  *result = gc_new_cons(argv->cons.car, argv->cons.cdr->cons.car);
  return TRUE;
}

PRIM(car)
{
  object o, p;
  if (argc != 1 && argc != 2) return FALSE;
  if (!listp(o = argv->cons.car)) return FALSE;
  if (argc == 1) {
    if (o == object_nil) *result = object_nil;
    else *result = o->cons.car;
  } else {
    if (o == object_nil) return FALSE;
    p = argv->cons.cdr->cons.car;
    o->cons.car = p;
    *result = p;
  }
  return TRUE;
}

PRIM(cdr)
{
  object o, p;
  if (argc != 1 && argc != 2) return FALSE;
  if (!listp(o = argv->cons.car)) return FALSE;
  if (argc == 1) {
    if (o == object_nil) *result = object_nil;
    else *result = o->cons.cdr;
  } else {
    if (o == object_nil) return FALSE;
    p = argv->cons.cdr->cons.car;
    o->cons.cdr = p;
    *result = p;
  }
  return TRUE;
}

// lambda

PRIM(lambda_parameter)
{
  object o;
  if (argc != 1) return FALSE;
  if (!typep(o = argv->cons.car, LAMBDA)) return FALSE;
  *result = o->lambda.params;
  return TRUE;
}

PRIM(lambda_body)
{
  object o;
  if (argc != 1) return FALSE;
  if (!typep(o = argv->cons.car, LAMBDA)) return FALSE;
  *result = o->lambda.body;
  return TRUE;
}

// number

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

PRIM(bit_and)
{
  int64_t x, y;
  if (argc != 2) return FALSE;
  if (!bi_int64(argv->cons.car, &x)) return FALSE;
  if (!bi_int64(argv->cons.cdr->cons.car, &y)) return FALSE;
  if (x < 0 || y < 0) return FALSE;
  *result = gc_new_xint(x & y);
  return TRUE;
}

PRIM(bit_or)
{
  int64_t x, y;
  if (argc != 2) return FALSE;
  if (!bi_int64(argv->cons.car, &x)) return FALSE;
  if (!bi_int64(argv->cons.cdr->cons.car, &y)) return FALSE;
  if (x < 0 || y < 0) return FALSE;
  *result = gc_new_xint(x | y);
  return TRUE;
}

PRIM(bit_xor)
{
  int64_t x, y;
  if (argc != 2) return FALSE;
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

PRIM(bit_shift)
{
  int64_t x, y;
  if (argc != 2) return FALSE;
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

// byte-array/array

PRIM(barray_new)
{
  int64_t size;
  if (argc != 1) return FALSE;
  if (!bi_int64(argv->cons.car, &size)) return FALSE;
  *result = gc_new_barray(BARRAY, size);
  return TRUE;
}

// TODO array new

#define LC(p) (*(unsigned char*)(p))

PRIM(array_access)
{
  int64_t i;
  object a, v;
  if (argc < 2 || argc > 3) return FALSE;
  a = argv->cons.car;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  switch (type(a)) {
    case BARRAY:
      if (i < 0 || i >= a->barray.size) return FALSE;
      if (argc == 2) *result = object_bytes[(unsigned char)(a->barray.elt[i])];
      else {
        if (!bytep(v = argv->cons.cdr->cons.car)) return FALSE;
        a->barray.elt[i] = (unsigned char)(v->xint.val);
        *result = v;
      }
      return TRUE;
    default:
      return FALSE;
  }
}

PRIM(array_copy)
{
  int64_t fp, tp, size;
  object from, to;
  if (argc != 5) return FALSE;
  from = argv->cons.car;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &fp))  return FALSE;
  to = (argv = argv->cons.cdr)->cons.car;
  if (type(from) != type(to)) return FALSE;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &tp))  return FALSE;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &size))  return FALSE;
  switch (type(from)) {
    case BARRAY:
      if (fp < 0 || tp < 0 || size <= 0) return FALSE;
      if ((fp + size) > from->barray.size) return FALSE;
      if ((tp + size) > to->barray.size) return FALSE;
      memmove(to->barray.elt + tp, from->barray.elt + fp, size);
      *result = to;
      return TRUE;
    default:
      return FALSE;
  }
}

#undef SPECIAL
#undef PRIM

#define SPECIAL(name) extern void special_##name(object, int, object);
#include "special.wk"
#undef SPECIAL

#define PRIM(name) extern int prim_##name(int, object, object *);
#include "prim.wk"
#undef PRIM

static char *symbol_name_map[] = {
  "add", "+",
  "array_access", "[]",
  "array_copy", "array-copy",
  "array_p", "array?",
  "assign", "<-",
  "atom_p", "atom?",
  "barray_new", "byte-array",
  "barray_p", "byte-array?",
  "bit_and", "bit-and",
  "bit_or", "bit-or",
  "bit_xor", "bit-xor",
  "bit_shift", "bit-shift",
  "equalp", "=",
  "integer_p", "integer?",
  "keyword_p", "keyword?",
  "lambda_parameter", "lambda-parameter",
  "lambda_body", "lambda-body",
  "macro_p", "macro?",
  "number_lt", "<",
  "number_modulo", "mod",
  "number_multiply", "*",
  "number_p", "number?",
  "operator_p", "operator?",
  "samep", "same?",
  "special_operator_p", "special-operator?",
  "string_p", "string?",
  "symbol_p", "symbol?",
  "to_barray", "->byte-array",
  "to_keyword", "->keyword",
  "to_string", "->string",
  "to_symbol", "->symbol",
  NULL
};

char *special_name_table[] = {
#define SPECIAL(name) #name,
#include "special.wk"
#undef SPECIAL
  NULL
};

char *prim_name_table[] = {
#define PRIM(name) #name,
#include "prim.wk"
#undef PRIM
  NULL
};

void (*special_table[])(object env, int argc, object argv) = {
#define SPECIAL(name) special_##name,
#include "special.wk"
#undef SPECIAL
  NULL
};

int (*prim_table[])(int argc, object argv, object *result) = {
#define PRIM(name) prim_##name,
#include "prim.wk"
#undef PRIM
  NULL
};

char *bi_as_symbol_name(char *name)
{
  int i;
  if (name == NULL) return NULL;
  for (i = 0; symbol_name_map[i] != NULL; i += 2)
    if (strcmp(name, symbol_name_map[i]) == 0) return symbol_name_map[i + 1];
  return name;
}

int bi_int64(object o, int64_t *p) {
  if (!typep(o, XINT)) return FALSE;
  *p = o->xint.val;
  return TRUE;
}

int bi_intptr(object o, intptr_t *p) {
  int64_t i;
  if (!bi_int64(o, &i)) return FALSE;
  if (i < INTPTR_MIN || i > INTPTR_MAX) return FALSE;
  *p = (intptr_t)i;
  return TRUE;
}

int bi_double(object o, double *p) {
  int64_t i;
  if (bi_int64(o, &i)) *p = (double)i;
  else if (typep(o, XFLOAT)) *p = o->xfloat.val;
  else return FALSE;
  return TRUE;
}
