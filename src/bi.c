// built in.

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

#undef SPECIAL
#undef PRIM

#define SPECIAL(name, sym) extern object special_##name(object, int, object);
#include "special.wk"
#undef SPECIAL

#define PRIM(name, sym) extern int prim_##name(int, object, object *);
#include "prim.wk"
#undef PRIM

char *special_name_table[] = {
#define SPECIAL(name, sym) #sym,
#include "special.wk"
#undef SPECIAL
  NULL
};

char *prim_name_table[] = {
#define PRIM(name, sym) #sym,
#include "prim.wk"
#undef PRIM
  NULL
};

object (*special_table[])(object env, int argc, object argv) = {
#define SPECIAL(name, sym) special_##name,
#include "special.wk"
#undef SPECIAL
  NULL
};

int (*prim_table[])(int argc, object argv, object *result) = {
#define PRIM(name, sym) prim_##name,
#include "prim.wk"
#undef PRIM
  NULL
};

object prim_xint(int64_t val)
{
  if (XINT_MIN <= val && val <= XINT_MAX) return gc_new_xint(val);
  return gc_new_xfloat(val);
}
