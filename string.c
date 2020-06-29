// string

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "splay.h"
#include "object.h"
#include "mem.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

DEFUN(string_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, STRING));
  return TRUE;
}

DEFUN(string_to_symbol)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, STRING, &o)) return FALSE;
  *result = gc_new_barray_from(SYMBOL, o->barray.elt, o->barray.size);
  return TRUE;
}
