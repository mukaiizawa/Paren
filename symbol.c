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

static int ensure_keyword(object o, object *result)
{
  if (!typep(o, KEYWORD)) {
    ip_mark_error("requires keyword");
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

PRIM(keyword_to_symbol)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_keyword(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray_from(SYMBOL, o->barray.size, o->barray.elt);
  return TRUE;
}

PRIM(keyword_to_string)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_keyword(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray(STRING, o->barray.size + 1);
  (*result)->barray.elt[0] = 0x3a;
  memcpy((*result)->barray.elt + 1, o->barray.elt, o->barray.size);
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
