// fix byte array

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

PRIM(barray_new)
{
  object x;
  if (argc != 1 || !typep((x = argv->cons.car), XINT)) return FALSE;
  *result = gc_new_barray(x->xint.val);
  return TRUE;
}
