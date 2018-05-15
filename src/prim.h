// built in

#define SPECIAL(name) object special_##name(object env, int argc, object argv)
#define PRIM(name) int prim_##name(int argc, object argv, object *result)

extern char *special_name_table[];
extern char *prim_name_table[];
extern int (*special_table[])(object env, int argc, object argv);
extern int (*prim_table[])(int argc, object argv, object *result);

extern object prim_xint(int64_t val);

