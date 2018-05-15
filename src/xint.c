// integer

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "bi.h"

#define XINT_ARG(n, v) \
{ \
  v = object_nth(argv, n); \
  if (!typep(v, Xint)) return FALSE; \
}

PRIM(xint_equal)
{
  object x, y;
  if (argc != 2) return FALSE;
  XINT_ARG(0, x);
  XINT_ARG(1, y);
  *result = object_bool(x->xint.val == y->xint.val);
  return TRUE;
}

PRIM(xint_add)
{
  object x, y;
  if (argc != 2) return FALSE;
  XINT_ARG(0, x);
  XINT_ARG(1, y);
  *result = prim_xint(x->xint.val + y->xint.val);
  return TRUE;
}

PRIM(xint_multiply)
{
  object x, y;
  if (argc != 2) return FALSE;
  XINT_ARG(0, x);
  XINT_ARG(1, y);
  *result = prim_xint(x->xint.val * y->xint.val);
  return TRUE;
}

PRIM(xint_eq)
{
  object x, y;
  if (argc != 2) return FALSE;
  XINT_ARG(0, x);
  XINT_ARG(1, y);
  *result = object_bool(x->xint.val == y->xint.val);
  return TRUE;
}

PRIM(xint_lt)
{
  object x, y;
  if (argc != 2) return FALSE;
  XINT_ARG(0, x);
  XINT_ARG(1, y);
  *result = object_bool(x->xint.val < y->xint.val);
  return TRUE;
}
