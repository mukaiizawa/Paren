// primitive

#define PRIM(name) int prim_##name(object args, object *result)

#define ARGC(n) \
{ \
  object ___ ## gensym; \
  n = 0; \
  for (___ ## gensym = args; ___ ## gensym != object_nil; ___ ## gensym = ___ ## gensym->cons.cdr) n++; \
}

#define ARG(n, p) \
{ \
  int i; \
  object ___ ## gensym; \
  ___ ## gensym = args; \
  for (i = n; i >= 0; i--) { \
    if (___ ## gensym == object_nil) return FALSE; \
    p = ___ ## gensym->cons.car; \
    ___ ## gensym = ___ ## gensym->cons.cdr; \
  } \
}

extern char *prim_name_table[];

extern object prim_xint(int64_t val);
