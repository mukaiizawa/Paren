// array/byte array

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

PRIM(array_size)
{
  object x;
  if (argc != 1) return FALSE;
  FETCH_ARG(x);
  switch (type(x)) {
    case BARRAY:
      *result = gc_new_xint(x->barray.size);
      return TRUE;
    default:
      return FALSE;
  }
  return TRUE;
}

PRIM(barray_new)
{
  object x;
  if (argc != 1) return FALSE;
  FETCH_ARG_AS(x, XINT);
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
  FETCH_ARG_AS(k, XINT);
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

PRIM(array_copy)
{
  int fp, tp, size;
  object from, ofp, to, otp, osize;
  if (argc != 5) return FALSE;
  FETCH_ARG(from);
  FETCH_ARG_AS(ofp, XINT);
  FETCH_ARG(to);
  if (type(from) != type(to)) return FALSE;
  FETCH_ARG_AS(otp, XINT);
  FETCH_ARG_AS(osize, XINT);
  fp = ofp->xint.val;
  tp = otp->xint.val;
  size = osize->xint.val;
  switch (type(from)) {
    case BARRAY:
      if (fp < 0 || tp < 0 || size <= 0) return FALSE;
      if ((fp + size) > from->barray.size) return FALSE;
      if ((tp + size) > to->barray.size) return FALSE;
      memmove(to->barray.elt + tp, from->barray.elt + fp, size);
      *result = to;
      return TRUE;
    default:
      return FALSE;
  }
}

PRIM(barray_to_symbol_keyword)
{
  object x;
  if (argc != 1) return FALSE;
  FETCH_ARG_AS(x, BARRAY);
  *result = gc_new_symbol(x->barray.elt);
  return TRUE;
}

#define x_to_barray(type) \
{ \
  int size; \
  object x; \
  if (argc != 1) return FALSE; \
  FETCH_ARG_AS(x, type); \
  *result = gc_new_barray(size = strlen(x->symbol.name)); \
  memcpy((*result)->barray.elt, x->symbol.name, size); \
  return TRUE; \
} \

PRIM(symbol_to_barray)
{
  x_to_barray(SYMBOL);
}

PRIM(keyword_to_barray)
{
  x_to_barray(KEYWORD);
}
