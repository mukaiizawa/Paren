// primitive

#include <stdlib.h>

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "prim.h"

#undef PRIM
char *prim_name_table[] = {
#define PRIM(n) "%" #n,
#include "pprim.wk"
#undef PRIM
  NULL
};

object prim_xint(int64_t val)
{
  object o;
  o = object_alloc();
  if(XINT_MIN <= val && val <= XINT_MAX) {
    o->header.type = xint;
    o->xint.val = val;
  } else {
    o->header.type = xfloat;
    o->xfloat.val = val;
  }
  return o;
}
