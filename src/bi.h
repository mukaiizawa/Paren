// built-in.

#include "xbarray.h"

#define DEFSP(name) int special_##name(int argc, object argv)
#define DEFUN(name) int function_##name(int argc, object argv, object *result)

extern struct xbarray bi_buf;

extern char *special_name_table[];
extern char *function_name_table[];
extern int (*special_table[])(int argc, object argv);
extern int (*function_table[])(int argc, object argv, object *result);
extern char *bi_as_symbol_name(char *name, char *buf);

extern int bi_argc_range(int argc, int min, int max);
extern int bi_arg_cons(object o, object *result);
extern int bi_arg_string(object o, object *result);
extern int bi_arg_func(object o, object *result);
extern int bi_arg_symbol(object o, object *result);
extern int bi_arg_bytes(object o, object *result);
extern int bi_arg_array(object o, object *result);
extern int bi_arg_dict(object o, object *result);
extern int bi_arg_keyword(object o, object *result);
extern int bi_arg_bytes_like(object o, object *result);
extern int bi_arg_mutable_bytes_like(object o, object *result);
extern int bi_arg_immutable_bytes_like(object o, object *result);
extern int bi_arg_proc(object o, object *result);
extern int bi_arg_list(object o, object *result);
extern int bi_cint(object o, int *p);
extern int bi_cpint(object o, int *p);
extern int bi_cint64(object o, int64_t *p);
extern int bi_cintptr(object o, intptr_t *p);
extern int bi_cdouble(object o, double *p);
extern int bi_cstrings(int n, object argv, char **ss);
extern int bi_cstring(object argv, char **ss);
