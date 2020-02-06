// built-in.

#include "std.h"

#include <math.h>

#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

int bi_argc_range(int argc, int min, int max)
{
  if (argc < min) ip_mark_error("too few arguments");
  else if ((!min && !max && argc != 0) || (max && argc > max))
    ip_mark_error("too many arguments");
  else return TRUE;
  return FALSE;
}

static int arg(object o, int test, object *result)
{
  if (!test) {
    ip_mark_error("illegal argument");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

int bi_arg_type(object o, int type, object *result)
{
  return arg(o, type_p(o, type), result);
}

int bi_arg_list(object o, object *result)
{
  return arg(o, list_p(o), result);
}

int bi_arg_barray(object o, object *result)
{
  return arg(o, barray_p(o), result);
}

int bi_arg_lambda(object o, object *result)
{
  return arg(o, (type_p(o, LAMBDA) || type_p(o, MACRO)), result);
}

int bi_arg_fp(object o, FILE **result)
{
  return bi_intptr(o, (intptr_t *)result);
}

DEFUN(samep)
{
  object o;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  o = argv->cons.car;
  *result = object_true;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (o != argv->cons.car) {
      *result = object_nil;
      break;
    }
  }
  return TRUE;
}

DEFUN(address)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = gc_new_xint((intptr_t)argv->cons.car);
  return TRUE;
}

DEFUN(not)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(argv->cons.car == object_nil);
  return TRUE;
}

DEFUN(gensym)
{
  static int c = 0;
  struct xbarray x;
  xbarray_init(&x);
  xbarray_addf(&x, "$G-%d", ++c);
  *result = gc_new_barray_from(SYMBOL, x.elt, x.size);
  xbarray_free(&x);
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

static char *symbol_name_map[] = {
  // ip/DEFSP
  "symbol_bind", "<-",
  "unwind_protect", "unwind-protect",
  // ip/DEFUN
  "bound_p", "bound?",
  "expand_macro", "expand-macro",
  "object_p", "object?",
  "is_a_p", "is-a?",
  "find_class", "find-class",
  "find_method", "find-method",
  // bi
  "samep", "same?",
  // sequence
  "array_p", "array?",
  "array_new", "array",
  "barray_copy", "byte-array-copy",
  "barray_index", "byte-array-index",
  "barray_new", "byte-array",
  "barray_p", "byte-array?",
  "barray_to_string", "byte-array->string",
  "cons_p", "cons?",
  "set_assoc", "assoc!",
  "keyword_p", "keyword?",
  "keyword_to_string", "keyword->string",
  "keyword_to_symbol", "keyword->symbol",
  "last_cons", "last-cons",
  "nth_set", "nth!",
  "set_car", "car!",
  "set_cdr", "cdr!",
  "string_equal", "string=",
  "string_p", "string?",
  "string_to_keyword", "string->keyword",
  "string_to_symbol", "string->symbol",
  "symbol_p", "symbol?",
  "symbol_to_keyword", "symbol->keyword",
  "symbol_to_string", "symbol->string",
  "to_barray", "->byte-array",
  "xreverse", "reverse!",
  // number
  "bit_and", "bit-and",
  "bit_or", "bit-or",
  "bit_shift", "bit-shift",
  "bit_xor", "bit-xor",
  "integer_p", "integer?",
  "number_add", "+",
  "number_ceiling", "ceiling",
  "number_divide", "/",
  "number_equal_p", "=",
  "number_floor", "floor",
  "number_lt", "<",
  "number_modulo", "mod",
  "number_multiply", "*",
  "number_p", "number?",
  "number_truncate", "truncate",
  // lambda
  "builtin_name", "builtin-name",
  "builtin_p", "builtin?",
  "lambda_body", "lambda-body",
  "function_p", "function?",
  "lambda_parameter", "lambda-parameter",
  "macro_p", "macro?",
  "special_operator_p", "special-operator?",
  // os
  "milli_time", "milli-time",
  NULL
};

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

char *bi_as_symbol_name(char *name)
{
  int i;
  if (name == NULL) return NULL;
  for (i = 0; symbol_name_map[i] != NULL; i += 2)
    if (strcmp(name, symbol_name_map[i]) == 0) return symbol_name_map[i + 1];
  return name;
}

int bi_int(object o, int *p) {
  if (!type_p(o, XINT)) return FALSE;
  *p = o->xint.val;
  return TRUE;
}

int bi_int64(object o, int64_t *p) {
  if (!type_p(o, XINT)) return FALSE;
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
  else if (type_p(o, XFLOAT)) *p = o->xfloat.val;
  else return FALSE;
  return TRUE;
}
