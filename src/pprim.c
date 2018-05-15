// primitive table for paren

#include "std.h"
#include "xsplay.h"
#include "object.h"

char *special_name_table[] = {
#define SPECIAL(name, sym) #sym,
#include "special.wk"
#undef SPECIAL
  NULL
};

#define SPECIAL(name, sym) extern object special_##name(object, int, object);
#include "special.wk"
#undef SPECIAL

object (*special_table[])(object env, int argc, object argv) = {
#define SPECIAL(name, sym) special_##name,
#include "special.wk"
#undef SPECIAL
  NULL
};

char *prim_name_table[] = {
#define PRIM(name) ":" #name,
#include "prim.wk"
#undef PRIM
  NULL
};

#define PRIM(name) extern int prim_##name(int, object, object *);
#include "prim.wk"
#undef PRIM

int (*prim_table[])(int argc, object argv, object *result) = {
#define PRIM(name) prim_##name,
#include "prim.wk"
#undef PRIM
  NULL
};
