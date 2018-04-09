// primitive

#define PRIM(name) int prim_##name(object *args, object *result)

#define ARG(n, p) \
{ \
  int i; \
  object o; \
  o = *args; \
  for (i = n; i >= 0; i--) { \
    if (o == object_nil) return FALSE; \
    p = o->cons.car; \
    o = o->cons.cdr; \
  } \
}

extern char *prim_name_table[];

extern object prim_xint(int64_t val);
