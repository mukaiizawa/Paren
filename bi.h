// built-in.

#define DEFSP(name) int special_##name(int argc, object argv)
#define DEFUN(name) int function_##name(int argc, object argv, object *result)

struct xbarray bi_buf;

extern char *special_name_table[];
extern char *function_name_table[];
extern int (*special_table[])(int argc, object argv);
extern int (*function_table[])(int argc, object argv, object *result);
extern char *bi_as_symbol_name(char *name);

extern int bi_argc_range(int argc, int min, int max);
extern int bi_arg_type(object o, int type, object *result);
extern int bi_arg_mem(object o, object *result);
extern int bi_arg_mutable_mem(object o, object *result);
extern int bi_arg_fp(object o, FILE **result);
extern int bi_arg_proc(object o, object *result);
extern int bi_arg_list(object o, object *result);

extern int bi_sint(object o, int *p);
extern int bi_int64(object o, int64_t *p);
extern int bi_intptr(object o, intptr_t *p);
extern int bi_double(object o, double *p);
extern int bi_strings(int n, object argv, char **ss);
extern char *bi_string(object argv);
