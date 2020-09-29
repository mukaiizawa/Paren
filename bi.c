// built-in.

#include "std.h"

#include <math.h>
#include <float.h>

#include "xarray.h"
#include "xbarray.h"
#include "at.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

struct xbarray bi_buf;

int bi_argc_range(int argc, int min, int max)
{
  if (argc < min)
    return ip_mark_error("too few arguments");
  if ((!min && !max && argc != 0) || (max && argc > max))
    return ip_mark_error("too many arguments");
  return TRUE;
}

static void mark_type_error(void)
{
  ip_mark_error("illegal argument type");
}

static int arg_type(object o, int test, object *result)
{
  if (!test) {
    mark_type_error();
    return FALSE;
  }
  *result = o;
  return TRUE;
}

int bi_arg_type(object o, int type, object *result)
{
  return arg_type(o, object_type_p(o, type), result);
}

int bi_arg_bytes(object o, object *result)
{
  switch (object_type(o)) {
    case BYTES:
    case STRING:
    case SYMBOL:
    case KEYWORD:
      return arg_type(o, TRUE, result);
    default:
      return arg_type(o, FALSE, result);
  }
}

int bi_arg_mutable_bytes(object o, object *result)
{
  switch (object_type(o)) {
    case BYTES:
    case STRING:
      return arg_type(o, TRUE, result);
    default:
      return arg_type(o, FALSE, result);
  }
}

int bi_arg_fp(object o, FILE **result)
{
  return bi_intptr(o, (intptr_t *)result);
}

int bi_arg_lambda(object o, object *result)
{
  switch (object_type(o)) {
    case LAMBDA:
    case MACRO:
      return arg_type(o, TRUE, result);
    default:
      return arg_type(o, FALSE, result);
  }
}

int bi_arg_list(object o, object *result)
{
  return arg_type(o, object_list_p(o), result);
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
  "find_class", "find-class",
  "find_method", "find-method",
  "is_a_p", "is-a?",
  "not", "!",
  "object_p", "object?",
  // bi
  "eq_p", "eq?",
  "neq_p", "neq?",
  // array
  "array_at", "[]",
  "array_put", "[]<-",
  "array_p", "array?",
  "array_copy", "array-copy",
  "array_new", "array",
  "array_length", "array-length",
  // bytes
  "bytes_concat", "bytes-concat",
  "bytes_copy", "bytes-copy",
  "bytes_eq_p", "bytes=",
  "bytes_index", "bytes-index",
  "bytes_length", "bytes-length",
  "bytes_new", "bytes",
  "bytes_p", "bytes?",
  "bytes_slice", "bytes-slice",
  "bytes_to_keyword", "bytes->keyword",
  "bytes_to_string", "bytes->string",
  "bytes_to_symbol", "bytes->symbol",
  "keyword_p", "keyword?",
  "string_p", "string?",
  "symbol_p", "symbol?",
  "to_bytes", "->bytes",
  "xbytes_to_string", "bytes->string!",
  // cons
  "cons_p", "cons?",
  "set_assoc", "assoc!",
  "last_cons", "last-cons",
  "nth_set", "nth!",
  "set_car", "car!",
  "set_cdr", "cdr!",
  "xreverse", "reverse!",
  // lambda
  "builtin_name", "builtin-name",
  "builtin_p", "builtin?",
  "lambda_body", "lambda-body",
  "function_p", "function?",
  "lambda_parameter", "lambda-parameter",
  "macro_p", "macro?",
  "special_operator_p", "special-operator?",
  // number
  "bit_and", "&",
  "bit_or", "|",
  "bit_shift", "<<",
  "bit_xor", "^",
  "int_p", "int?",
  "int_divide", "//",
  "number_add", "+",
  "number_divide", "/",
  "number_equal_p", "=",
  "number_lt", "<",
  "number_modulo", "mod",
  "number_multiply", "*",
  "number_p", "number?",
  "number_to_int", "number->int",
  // sock
  "server_socket", "server-socket",
  "client_socket", "client-socket",
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

int bi_sint(object o, int *p)
{
  if (!sint_p(o)) return ip_mark_error("expected integer");
  *p = sint_val(o);
  return TRUE;
}

int bi_int64(object o, int64_t *p)
{
  if (sint_p(o)) *p = sint_val(o);
  else if (object_type_p(o, XINT)) *p = o->xint.val;
  else return FALSE;
  return TRUE;
}

int bi_intptr(object o, intptr_t *p)
{
  int64_t i;
  if (bi_int64(o, &i)) {
    if (INTPTR_MIN <= i && i <= INTPTR_MAX) {
      *p = (intptr_t)i;
      return TRUE;
    }
  }
  mark_type_error();
  return FALSE;
}

int bi_double(object o, double *p)
{
  int64_t i;
  if (bi_int64(o, &i)) {
    *p = (double)i;
    return TRUE;
  }
  if (object_type_p(o, XFLOAT)) {
    *p = o->xfloat.val;
    return TRUE;
  }
  return FALSE;
}

int bi_strings(int n, object argv, char **ss)
{
  int i;
  object o;
  xbarray_reset(&bi_buf);
  for (i = 0; i < n; i++) {
    xassert(object_type_p(argv, CONS));
    o = argv->cons.car;
    if (!object_type_p(o, STRING)) {
      mark_type_error();
      return FALSE;
    }
    ss[i] = xbarray_reserve(&bi_buf, o->bytes.size);
    memcpy(ss[i], o->bytes.elt, o->bytes.size);
    xbarray_add(&bi_buf, '\0');
    argv = argv->cons.cdr;
  }
  return TRUE;
}

char *bi_string(object argv)
{
  char *result;
  if (!bi_strings(1, argv, &result)) return NULL;
  return result;
}
