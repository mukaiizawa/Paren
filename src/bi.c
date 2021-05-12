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
    return ip_mark_error("too few arguments");
  if ((!min && !max && argc != 0) || (max && argc > max))
    return ip_mark_error("too many arguments");
  return TRUE;
}

int bi_mark_type_error(void)
{
  ip_mark_error("illegal argument type");
  return FALSE;
}

static int arg_type(object o, int test, object *result)
{
  if (!test) return bi_mark_type_error();
  *result = o;
  return TRUE;
}

int bi_arg_type(object o, int type, object *result)
{
  return arg_type(o, object_type_p(o, type), result);
}

int bi_arg_mem(object o, object *result)
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

int bi_arg_mutable_mem(object o, object *result)
{
  switch (object_type(o)) {
    case BYTES:
    case STRING:
      return arg_type(o, TRUE, result);
    default:
      return arg_type(o, FALSE, result);
  }
}

int bi_arg_immutable_mem(object o, object *result)
{
  switch (object_type(o)) {
    case SYMBOL:
    case KEYWORD:
      return arg_type(o, TRUE, result);
    default:
      return arg_type(o, FALSE, result);
  }
}

int bi_arg_proc(object o, object *result)
{
  switch (object_type(o)) {
    case FUNC:
    case MACRO:
      return arg_type(o, TRUE, result);
    default:
      return arg_type(o, FALSE, result);
  }
}

int bi_arg_list(object o, object *result)
{
  return arg_type(o, list_p(o), result);
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
  "eq_p", "=",
  "same_p", "==",
  "at", "[]",
  "expand_macro", "expand-macro",
  "find_class", "find-class",
  "find_method", "find-method",
  "is_a_p", "is-a?",
  "in_p", "in?",
  "not", "!",
  // array
  "array_p", "array?",
  // dict
  "dict_access", "{}",
  "dict_p", "dict?",
  // mem
  "bytes_p", "bytes?",
  "keyword_p", "keyword?",
  "string_p", "string?",
  "symbol_p", "symbol?",
  "xstring", "string!",
  "ascii_p", "ascii?",
  "alnum_p", "alnum?",
  "alpha_p", "alpha?",
  "digit_p", "digit?",
  "space_p", "space?",
  "print_p", "print?",
  "lower_p", "lower?",
  "upper_p", "upper?",
  // cons
  "cons_p", "cons?",
  "last_cons", "last-cons",
  "nth_set", "nth!",
  "set_car", "car!",
  "set_cdr", "cdr!",
  "xreverse", "reverse!",
  // proc
  "builtin_name", "builtin-name",
  "builtin_p", "builtin?",
  "function_p", "function?",
  "macro_p", "macro?",
  "special_operator_p", "special-operator?",
  // number
  "bit_and", "&",
  "bit_not", "~",
  "bit_or", "|",
  "bit_shift", "<<",
  "bit_xor", "^",
  "int_p", "int?",
  "int_divide", "//",
  "number_add", "+",
  "number_divide", "/",
  "number_lt", "<",
  "number_modulo", "%",
  "number_multiply", "*",
  "number_p", "number?",
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

int bi_spint(object o, int *p)
{
  if (!bi_sint(o, p) || *p < 0) return ip_mark_error("expected positive integer");
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
  return bi_mark_type_error();
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

#define MAX_STRINGS 2

int bi_strings(int n, object argv, char **ss)
{
  int i;
  int offset[MAX_STRINGS]; // xbarray use realloc.
  object o;
  xassert(n <= MAX_STRINGS);
  xassert(object_type_p(argv, CONS));
  xbarray_reset(&bi_buf);
  for (i = 0; i < n; i++) {
    o = argv->cons.car;
    argv = argv->cons.cdr;
    if (!object_type_p(o, STRING)) {
      bi_mark_type_error();
      return FALSE;
    }
    offset[i] = bi_buf.size;
    memcpy(xbarray_reserve(&bi_buf, o->mem.size), o->mem.elt, o->mem.size);
    xbarray_add(&bi_buf, '\0');
  }
  for(i = 0; i < n; i++) ss[i] = bi_buf.elt + offset[i];
  return TRUE;
}

char *bi_string(object argv)
{
  char *result;
  if (!bi_strings(1, argv, &result)) return NULL;
  return result;
}
