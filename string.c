// string

#include "std.h"
#include "xsplay.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int ensure_string(object o, object *result)
{
  if (!typep(o, STRING)) {
    ip_mark_error("requires string");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

PRIM(string_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, STRING));
  return TRUE;
}

PRIM(string_add)
{
  object x, y;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  if (!ensure_string(argv->cons.cdr->cons.car, &y)) return FALSE;
  *result = gc_new_barray(STRING, x->barray.size + y->barray.size);
  memcpy((*result)->barray.elt, x->barray.elt, x->barray.size);
  memcpy((*result)->barray.elt + x->barray.size, y->barray.elt, y->barray.size);
  return TRUE;
}

PRIM(string_equal)
{
  int b;
  object x, y;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  if (!ensure_string(argv->cons.cdr->cons.car, &y)) return FALSE;
  if (x->barray.size != y->barray.size) b = FALSE;
  else b = memcmp(x->barray.elt, y->barray.elt, x->barray.size) == 0;
  *result = object_bool(b);
  return TRUE;
}

PRIM(string_to_barray)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  *result = gc_new_barray_from(BARRAY, x->barray.size, x->barray.elt);
  return TRUE;
}

PRIM(string_to_symbol)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  *result = gc_new_barray_from(SYMBOL, x->barray.size, x->barray.elt);
  return TRUE;
}