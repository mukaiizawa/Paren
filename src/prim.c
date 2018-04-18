// primitive

#include <stdlib.h>

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "prim.h"
#include "gc.h"

#undef PRIM
char *prim_name_table[] = {
#define PRIM(n) "%" #n,
#include "pprim.wk"
#undef PRIM
  NULL
};

object prim_xint(int64_t val)
{
  if(XINT_MIN <= val && val <= XINT_MAX) return gc_new_xint(val);
  return gc_new_xfloat(val);
}
