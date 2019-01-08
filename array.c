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
  *result = gc_new_barray(size);
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

PRIM(barray_to_symbol_keyword)
{
  char *s;
  object x;
  if (argc != 1) return FALSE;
  if (!typep(x = argv->cons.car, BARRAY)) return FALSE;
  s = xmalloc(x->barray.size + 1);
  memcpy(s, x->barray.elt, x->barray.size);
  s[x->barray.size] = '\0';
  *result = gc_new_symbol(s);
  return TRUE;
}

#define x_to_barray(type) \
{ \
  int size; \
  object x; \
  if (argc != 1) return FALSE; \
  if (!typep(x = argv->cons.car, type)) return FALSE; \
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
