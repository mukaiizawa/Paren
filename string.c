// string

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int ensure_string(object o, object *result)
{
  if (!typep(o, STRING)) {
    ip_mark_exception("requires string");
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

static void string_add (object y, object *result)
{
  object x;
  x = *result;
  *result = gc_new_barray(STRING, x->barray.size + y->barray.size);
  memcpy((*result)->barray.elt, x->barray.elt, x->barray.size);
  memcpy((*result)->barray.elt + x->barray.size, y->barray.elt, y->barray.size);
}

PRIM(string_add)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, FALSE)) return FALSE;
  if (!ensure_string(argv->cons.car, result)) return FALSE;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!ensure_string(argv->cons.car, &x)) return FALSE;
    string_add(x, result);
  }
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

PRIM(string_to_keyword)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  *result = gc_new_barray_from(KEYWORD, x->barray.size, x->barray.elt);
  return TRUE;
}

PRIM(string_byte_length)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  *result = gc_new_xint(x->barray.size);
  return TRUE;
}
