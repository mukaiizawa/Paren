// symbol/keyword

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int ensure_symbol(object o, object *result)
{
  if (!typep(o, SYMBOL)) {
    ip_mark_error("requires symbol");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

PRIM(symbol_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, SYMBOL));
  return TRUE;
}

PRIM(keyword_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, KEYWORD));
  return TRUE;
}

PRIM(symbol_to_string)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_symbol(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray_from(STRING, o->barray.size, o->barray.elt);
  return TRUE;
}

PRIM(symbol_to_keyword)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_symbol(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray_from(KEYWORD, o->barray.size, o->barray.elt);
  return TRUE;
}
