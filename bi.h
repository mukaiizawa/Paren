// built-in.

#define SPECIAL(name) void special_##name(object env, int argc, object argv)
#define PRIM(name) int prim_##name(int argc, object argv, object *result)

#define FETCH_ARG(x) { \
  x = argv->cons.car; \
  argv = argv->cons.cdr; \
}

#define FETCH_ARG_AS(x, type) { \
  FETCH_ARG(x); \
  if (!typep(x, type)) return FALSE; \
}

#define FETCH_NUMBER(x) { \
  FETCH_ARG(x); \
  if (!numberp(x)) return FALSE; \
}

#define FETCH_BYTE(x) { \
  FETCH_ARG(x); \
  if (!bytep(x)) return FALSE; \
}

extern char *special_name_table[];
extern char *prim_name_table[];
extern void (*special_table[])(object env, int argc, object argv);
extern int (*prim_table[])(int argc, object argv, object *result);
extern char *bi_as_symbol_name(char *name);
