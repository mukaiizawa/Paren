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
  switch (type(x = argv->cons.car)) {
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
  int64_t size;
  if (argc != 1) return FALSE;
  if (!bi_int64(argv->cons.car, &size)) return FALSE;
  *result = gc_new_barray(BARRAY, size);
  return TRUE;
}

PRIM(array_access)
{
  int64_t i;
  object a, v;
  if (argc < 2 || argc > 3) return FALSE;
  a = argv->cons.car;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &i))  return FALSE;
  switch (type(a)) {
    case BARRAY:
      if (i < 0 || i >= a->barray.size) return FALSE;
      if (argc == 2) *result = object_bytes[(int)(a->barray.elt[i])];
      else {
        if (!bytep(v = argv->cons.cdr->cons.car)) return FALSE;
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
  int64_t fp, tp, size;
  object from, to;
  if (argc != 5) return FALSE;
  from = argv->cons.car;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &fp))  return FALSE;
  to = (argv = argv->cons.cdr)->cons.car;
  if (type(from) != type(to)) return FALSE;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &tp))  return FALSE;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &size))  return FALSE;
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

PRIM(barray_to_symbol)
{
  object x;
  if (argc != 1) return FALSE;
  if (!typep(x = argv->cons.car, BARRAY)) return FALSE;
  if (x->barray.elt[0] == ':') return FALSE;
  *result = gc_new_barray_from(SYMBOL, x->barray.size, x->barray.elt);
  return TRUE;
}

PRIM(barray_to_keyword)
{
  object x;
  if (argc != 1) return FALSE;
  if (!typep(x = argv->cons.car, BARRAY)) return FALSE;
  if (x->barray.elt[0] != ':') return FALSE;
  *result = gc_new_barray_from(KEYWORD, x->barray.size, x->barray.elt);
  return TRUE;
}

PRIM(barray_to_string)
{
  object x;
  if (argc != 1) return FALSE;
  if (!typep(x = argv->cons.car, BARRAY)) return FALSE;
  *result = gc_new_barray_from(STRING, x->barray.size, x->barray.elt);
  return TRUE;
}

PRIM(to_barray)
{
  object x;
  if (argc != 1) return FALSE;
  switch (type(x = argv->cons.car)) {
    case BARRAY:
      *result = x;
      break;
    case SYMBOL:
    case KEYWORD:
    case STRING:
      *result = gc_new_barray_from(BARRAY, x->barray.size, x->barray.elt);
      break;
    default: return FALSE;
  }
  return TRUE;
}
