// primitive

int (*prim_table[])(object args, object *result) = {
#define PRIM(name) prim_##name,
#include "prim_name.wk"
#undef PRIM
  NULL
};

char *prim_name_table[] = {
#define PRIM(n) #n,
#include "prim_name.wk"
#undef PRIM
  NULL
};
