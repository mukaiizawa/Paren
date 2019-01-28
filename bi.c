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

PRIM(byte_array_p)
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
      case STRING: // TODO count as utf-8
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
  o = object_nth(argv, 0);
  if ((argc != 1 && argc != 2) || !listp(o)) return FALSE;
  if (argc == 1) {
    if (o == object_nil) *result = object_nil;
    else *result = o->cons.car;
  } else {
    if (o == object_nil) return FALSE;
    p = object_nth(argv, 1);
    o->cons.car = p;
    *result = p;
  }
  return TRUE;
}

PRIM(cdr)
{
  object o, p;
  o = object_nth(argv, 0);
  if ((argc != 1 && argc != 2) || !listp(o)) return FALSE;
  if (argc == 1) {
    if (o == object_nil) *result = object_nil;
    else *result = o->cons.cdr;
  } else {
    if (o == object_nil) return FALSE;
    p = object_nth(argv, 1);
    o->cons.cdr = p;
    *result = p;
  }
  return TRUE;
}

// number

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

// byte-array/array

PRIM(barray_new)
{
  int64_t size;
  if (argc != 1) return FALSE;
  if (!bi_int64(argv->cons.car, &size)) return FALSE;
  *result = gc_new_barray(BARRAY, size);
  return TRUE;
}

PRIM(array_access)
{
  int64_t i;
  object a, v;
  if (argc < 2 || argc > 3) return FALSE;
  a = argv->cons.car;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &i))  return FALSE;
  switch (type(a)) {
    case BARRAY:
      if (i < 0 || i >= a->barray.size) return FALSE;
      if (argc == 2) *result = object_bytes[(int)(a->barray.elt[i])];
      else {
        if (!bytep(v = argv->cons.cdr->cons.car)) return FALSE;
        a->barray.elt[i] = v->xint.val;
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

PRIM(barray_to_symbol)
{
  object x;
  if (argc != 1) return FALSE;
  if (!typep(x = argv->cons.car, BARRAY)) return FALSE;
  if (x->barray.elt[0] == ':') return FALSE;
  *result = gc_new_barray_from(SYMBOL, x->barray.size, x->barray.elt);
  return TRUE;
}

PRIM(barray_to_keyword)
{
  object x;
  if (argc != 1) return FALSE;
  if (!typep(x = argv->cons.car, BARRAY)) return FALSE;
  if (x->barray.elt[0] != ':') return FALSE;
  *result = gc_new_barray_from(KEYWORD, x->barray.size, x->barray.elt);
  return TRUE;
}

PRIM(barray_to_string)
{
  object x;
  if (argc != 1) return FALSE;
  if (!typep(x = argv->cons.car, BARRAY)) return FALSE;
  *result = gc_new_barray_from(STRING, x->barray.size, x->barray.elt);
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
    default: return FALSE;
  }
  return TRUE;
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
  "array_access", "[]",
  "array_copy", "array-copy",
  "array_p", "array?",
  "assign", "<-",
  "atom_p", "atom?",
  "barray_new", "byte-array",
  "barray_to_keyword", "byte-array->keyword",
  "barray_to_string", "byte-array->string",
  "barray_to_symbol", "byte-array->symbol",
  "byte_array_p", "byte-array?",
  "equalp", "=",
  "keyword_p", "keyword?",
  "macro_p", "macro?",
  "number_add", "+",
  "number_lt", "<",
  "number_modulo", "mod",
  "number_multiply", "*",
  "number_p", "number?",
  "number_to_integer", "number->integer",
  "number_to_string", "number->string",
  "operator_p", "operator?",
  "samep", "same?",
  "special_operator_p", "special-operator?",
  "string_p", "string?",
  "symbol_p", "symbol?",
  "throw", "basic-throw",
  "to_barray", "->byte-array",
  "try", "basic-try",
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
