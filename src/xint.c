// integer

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "prim.h"

#define XINT_ARG(n, arg) \
{ \
  ARG(n, arg); \
  arg = eval(env, arg); \
  if (!typep(arg, Xint)) return FALSE; \
}

PRIM(xint_equal)
{
  int argc;
  object x, y;
  ARGC(argc);
  if (argc != 2) return FALSE;
  XINT_ARG(0, x);
  XINT_ARG(1, y);
  *result = object_bool(x->xint.val == y->xint.val);
  return TRUE;
}

PRIM(xint_add)
{
  int argc;
  object x, y;
  ARGC(argc);
  if (argc != 2) return FALSE;
  XINT_ARG(0, x);
  XINT_ARG(1, y);
  *result = prim_xint(x->xint.val + y->xint.val);
  return TRUE;
}
