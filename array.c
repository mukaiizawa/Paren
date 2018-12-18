// array/byte array

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"


PRIM(barray_new)
{
  object x;
  if (argc != 1) return FALSE;
  FETCH_INTEGER(x);
  *result = gc_new_barray(x->xint.val);
  return TRUE;
}

PRIM(array_new)
{
  object x;
  if (argc != 1 || !typep((x = argv->cons.car), XINT)) return FALSE;
  *result = gc_new_barray(x->xint.val);
  return TRUE;
}

// refer/assign
PRIM(array_access)
{
  int i;
  object a, k, v;
  if (argc < 2 || argc > 3) return FALSE;
  a = argv->cons.car;
  argv = argv->cons.cdr;
  FETCH_INTEGER(k);
  i = k->xint.val;
  switch (type(a)) {
    case BARRAY:
      if (i < 0 || i >= a->barray.size) return FALSE;
      if (argc == 2) *result = object_bytes[(int)(a->barray.elt[i])];
      else {
        FETCH_BYTE(v);
        a->barray.elt[i] = v->xint.val;
        *result = v;
      }
      return TRUE;
    default:
      return FALSE;
  }
}
