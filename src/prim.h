// primitive

#define PRIM(name) int prim_##name(object args, object *result)

#define ARGC(argc) \
{ \
  object o; \
  argc = 0; \
  for (o = args; o != object_nil; o = o->cons.cdr) argc++; \
}

#define ARG(n, arg) \
{ \
  int i; \
  arg = args; \
  for (i = n; i > 0; i--) { \
    if (arg == object_nil) return FALSE; \
    arg = arg->cons.cdr; \
  } \
  arg = arg->cons.car; \
}

extern char *prim_name_table[];

extern object prim_xint(int64_t val);
