// built-in.

#include "xbarray.h"

#define DEFSP(name) int special_##name(int argc, object argv)
#define DEFUN(name) int function_##name(int argc, object argv, object *result)

#define BI_SYM    0x001
#define BI_STR    0x002
#define BI_ARRAY  0x004
#define BI_BYTES  0x008
#define BI_CONS   0x010
#define BI_DICT   0x020
#define BI_FUNC   0x040
#define BI_MACRO  0x080
#define BI_SP     0x100
#define BI_LIST   0x200
#define BI_NUM    0x400

extern struct xbarray bi_buf;

extern char *special_name_table[];
extern char *function_name_table[];
extern int (*special_table[])(int argc, object argv);
extern int (*function_table[])(int argc, object argv, object *result);

extern int bi_argc_range(int argc, int min, int max);
extern int bi_argv(int expected_types, object o, object *result);
extern int bi_range(int min, int x, int max);

extern int bi_cbyte(object o, int *p);
extern int bi_cdouble(object o, double *p);
extern int bi_cint(object o, int *p);
extern int bi_cint64(object o, int64_t *p);
extern int bi_cintptr(object o, intptr_t *p);
extern int bi_cpint(object o, int *p);
extern int bi_cstring(object argv, char **ss);
extern int bi_cstrings(int n, object argv, char **ss);
