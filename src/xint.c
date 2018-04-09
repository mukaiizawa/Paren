// integer

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "prim.h"

#define XINT_ARG(n, x) \
{ \
  object xa; \
  ARG(n, xa); \
  if (xa->header.type != xint) return FALSE; \
  x = xa->xint.val; \
}

PRIM(xint_equal)
{
  int x, y;
  XINT_ARG(0, x);
  XINT_ARG(1, y);
  *result = object_bool(x == y);
  return TRUE;
}

PRIM(xint_add)
{
  int x, y;
  XINT_ARG(0, x);
  XINT_ARG(1, y);
  *result = prim_xint(x + y);
  return TRUE;
}
