// built-in.

#define SPECIAL(name) void special_##name(object env, int argc, object argv)
#define PRIM(name) int prim_##name(int argc, object argv, object *result)

#define FETCH_NUMBER(x) { \
  x = argv->cons.car; \
  if (!numberp(x)) return FALSE; \
  argv = argv->cons.cdr; \
}

#define FETCH_INTEGER(x) { \
  x = argv->cons.car; \
  if (!typep(x, XINT)) return FALSE; \
  argv = argv->cons.cdr; \
}

#define FETCH_BYTE(x) { \
  x = argv->cons.car; \
  if (!bytep(x)) return FALSE; \
  argv = argv->cons.cdr; \
}

extern char *special_name_table[];
extern char *prim_name_table[];
extern void (*special_table[])(object env, int argc, object argv);
extern int (*prim_table[])(int argc, object argv, object *result);
extern char *bi_as_symbol_name(char *name);
