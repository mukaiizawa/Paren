// built-in.

#define DEFSP(name) int special_##name(int argc, object argv)
#define DEFUN(name) int function_##name(int argc, object argv, object *result)

extern char *special_name_table[];
extern char *function_name_table[];
extern int (*special_table[])(int argc, object argv);
extern int (*function_table[])(int argc, object argv, object *result);
extern char *bi_as_symbol_name(char *name);

extern int bi_int(object o, int *p);
extern int bi_int64(object o, int64_t *p);
extern int bi_intptr(object o, intptr_t *p);
extern int bi_double(object o, double *p);
