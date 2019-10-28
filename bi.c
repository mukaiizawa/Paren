// built-in.

#include "std.h"

#include <math.h>

#include "xsplay.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

// basic built-in

PRIM(samep)
{
  int b;
  object o;
  if (!ip_ensure_arguments(argc, 2, FALSE)) return FALSE;
  o = argv->cons.car;
  while ((argv = argv->cons.cdr) != object_nil)
    if (!(b = (o == argv->cons.car))) break;
  *result = object_bool(b);
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
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(special_p(argv->cons.car));
  return TRUE;
}

PRIM(operator_p)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  o = argv->cons.car;
  *result = object_bool(prim_p(o) || typep(o, LAMBDA));
  return TRUE;
}

PRIM(gensym)
{
  static int c = 0;
  struct xbarray x;
  xbarray_init(&x);
  xbarray_addf(&x, "#G%d", ++c);
  *result = gc_new_barray_from(SYMBOL, x.size, x.elt);
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

#undef SPECIAL
#undef PRIM

#define SPECIAL(name) extern int special_##name(int, object);
#include "special.wk"
#undef SPECIAL

#define PRIM(name) extern int prim_##name(int, object, object *);
#include "prim.wk"
#undef PRIM

static char *symbol_name_map[] = {
  "symbol_bind", "symbol-bind",
  "samep", "same?",
  "operator_p", "operator?",
  "special_operator_p", "special-operator?",

  "cons_p", "cons?",

  "bit_and", "bit-and",
  "bit_or", "bit-or",
  "bit_shift", "bit-shift",
  "bit_xor", "bit-xor",
  "integer_p", "integer?",
  "number_add", "number+",
  "number_equal_p", "number=",
  "number_lt", "<",
  "number_modulo", "mod",
  "number_multiply", "*",
  "number_p", "number?",
  "number_to_string", "number->string",

  "string_add", "string+",
  "string_equal", "string=",
  "string_p", "string?",
  "string_to_barray", "string->byte-array",
  "string_to_symbol", "string->symbol",

  "array_access", "[]",
  "array_copy", "array-copy",
  "array_length", "array-length",
  "array_p", "array?",
  "barray_length", "byte-array-length",
  "barray_new", "byte-array",
  "barray_p", "byte-array?",
  "barray_to_string", "byte-array->string",

  "keyword_p", "keyword?",
  "symbol_p", "symbol?",
  "symbol_to_keyword", "symbol->keyword",
  "symbol_to_string", "symbol->string",

  "lambda_parameter", "lambda-parameter",
  "lambda_body", "lambda-body",
  "macro_p", "macro?",

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

int (*special_table[])(int argc, object argv) = {
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
