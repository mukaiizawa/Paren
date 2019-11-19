// array/byte-array

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int ensure_byte_array(object o, object *result)
{
  if (!typep(o, BARRAY)) {
    ip_mark_error("requires byte-array");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

static int ensure_array(object o, object *result)
{
  if (!typep(o, ARRAY)) {
    ip_mark_error("requires array");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

PRIM(array_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, ARRAY));
  return TRUE;
}

PRIM(barray_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, BARRAY));
  return TRUE;
}

PRIM(barray_new)
{
  int size;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!bi_int(argv->cons.car, &size)) return FALSE;
  *result = gc_new_barray(BARRAY, size);
  return TRUE;
}

static int barray_access(object a, int i, object v, object *result)
{
  if (i >= a->barray.size) return FALSE;
  if (v == NULL) *result = gc_new_xint((unsigned char)(a->barray.elt[i]));
  else {
    if (!bytep(v)) return FALSE;
    a->barray.elt[i] = (char)(v->xint.val);
    *result = v;
  }
  return TRUE;
}

PRIM(array_access)
{
  int i;
  object a, v;
  if (!ip_ensure_arguments(argc, 2, 3)) return FALSE;
  a = argv->cons.car;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &i) || i < 0) {
    ip_mark_error("array index must be positive number");
    return FALSE;
  }
  if (argc == 2) v = NULL;
  else v = argv->cons.cdr->cons.car;
  switch (type(a)) {
    // case ARRAY
    //   return barray_access(a, i, v, result);
    case BARRAY:
      return barray_access(a, i, v, result);
    default:
      ip_mark_error("requires array/byte-array");
      return FALSE;
  }
}

PRIM(barray_length)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_byte_array(argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint(argv->cons.car->array.size);
  return TRUE;
}

PRIM(array_length)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_array(argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint(argv->cons.car->array.size);
  return TRUE;
}

PRIM(array_copy)
{
  int fp, tp, size;
  object from, to;
  if (!ip_ensure_arguments(argc, 5, 5)) return FALSE;
  from = argv->cons.car;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &fp)) {
    ip_mark_error("source array index must be integer");
    return FALSE;
  }
  to = (argv = argv->cons.cdr)->cons.car;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &tp)) {
    ip_mark_error("destination array index must be integer");
    return FALSE;
  }
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &size)) {
    ip_mark_error("copy size must be integer");
    return FALSE;
  }
  switch (type(from)) {
    case BARRAY:
      if (!typep(to, BARRAY))
        ip_mark_error("destination must be byte-array");
      else if (fp < 0)
        ip_mark_error("source array index must be positive");
      else if (tp < 0)
        ip_mark_error("destination array index must be positive");
      else if (size <= 0)
        ip_mark_error("copy size must be positive");
      else if ((fp + size) > from->barray.size)
        ip_mark_error("source array index out of bounds error");
      else if ((tp + size) > to->barray.size)
        ip_mark_error("destination array index out of bounds error");
      else {
        memmove(to->barray.elt + tp, from->barray.elt + fp, size);
        *result = to;
        return TRUE;
      }
      return FALSE;
    default:
      return FALSE;
  }
}

PRIM(barray_to_string)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_byte_array(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray_from(STRING, o->barray.size, o->barray.elt);
  return TRUE;
}
