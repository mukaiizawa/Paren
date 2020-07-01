// built-in.

#include "std.h"

#include <math.h>

#include "xarray.h"
#include "xbarray.h"
#include "splay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

struct xbarray bi_buf;

int bi_argc_range(int argc, int min, int max)
{
  if (argc < min) ip_mark_error("too few arguments");
  else if ((!min && !max && argc != 0) || (max && argc > max))
    ip_mark_error("too many arguments");
  else return TRUE;
  return FALSE;
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

int bi_arg_barray(object o, object *result)
{
  switch (object_type(o)) {
    case BARRAY:
    case STRING:
    case SYMBOL:
    case KEYWORD:
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
  "object_p", "object?",
  "is_a_p", "is-a?",
  "find_class", "find-class",
  "find_method", "find-method",
  // bi
  "eq_p", "eq?",
  "neq_p", "neq?",
  // array
  "array_at", "array-at",
  "array_p", "array?",
  "array_new", "array",
  "array_length", "array-length",
  "array_put", "array-at!",
  // byte-array
  "barray_at", "byte-array-at",
  "barray_concat", "byte-array-concat",
  "barray_copy", "byte-array-copy",
  "barray_index", "byte-array-index",
  "barray_length", "byte-array-length",
  "barray_new", "byte-array",
  "barray_p", "byte-array?",
  "barray_put", "byte-array-at!",
  "barray_to_string", "byte-array->string",
  "barray_unmatch_index", "byte-array-unmatch-index",
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
  "integer_p", "integer?",
  "number_add", "+",
  "number_ceiling", "Math.ceiling",
  "number_divide", "/",
  "number_equal_p", "=",
  "number_floor", "Math.floor",
  "number_lt", "<",
  "number_modulo", "mod",
  "number_multiply", "*",
  "number_p", "number?",
  "number_truncate", "Math.truncate",
  // string
  "string_p", "string?",
  "string_to_symbol", "string->symbol",
  // symbol
  "keyword_p", "keyword?",
  "keyword_to_symbol", "keyword->symbol",
  "symbol_p", "symbol?",
  "symbol_to_keyword", "symbol->keyword",
  "symbol_to_string", "symbol->string",
  "to_barray", "->byte-array",
  // os
  "os_fp", "OS.fp",
  "os_fopen", "OS.fopen",
  "os_fgetc", "OS.fgetc",
  "os_fputc", "OS.fputc",
  "os_fgets", "OS.fgets",
  "os_fread", "OS.fread",
  "os_fwrite", "OS.fwrite",
  "os_fseek", "OS.fseek",
  "os_ftell", "OS.ftell",
  "os_fclose", "OS.fclose",
  "os_stat", "OS.stat",
  "os_utime", "OS.utime",
  "os_getcwd", "OS.getcwd",
  "os_chdir", "OS.chdir",
  "os_readdir", "OS.readdir",
  "os_remove", "OS.remove",
  "os_mkdir", "OS.mkdir",
  "os_rename", "OS.rename",
  "os_time", "OS.time",
  "os_timediff", "OS.timediff",
  "os_clock", "OS.clock",
  "os_sleep", "OS.sleep",
  "os_system", "OS.system",
  "os_getenv", "OS.getenv",
  "os_putenv", "OS.putenv",
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
  if (!sint_p(o)) {
    ip_mark_error("illegal argument type");
    return FALSE;
  }
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
  if (bi_int64(o, &i)) *p = (double)i;
  else if (object_type_p(o, XFLOAT)) *p = o->xfloat.val;
  else return FALSE;
  return TRUE;
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
    ss[i] = xbarray_reserve(&bi_buf, o->barray.size);
    memcpy(ss[i], o->barray.elt, o->barray.size);
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
