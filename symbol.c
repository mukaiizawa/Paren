// symbol

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "splay.h"
#include "object.h"
#include "mem.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

DEFUN(symbol_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, SYMBOL));
  return TRUE;
}

DEFUN(symbol_to_string)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &o)) return FALSE;
  *result = gc_new_bytes_from(STRING, o->bytes.elt, o->bytes.size);
  return TRUE;
}

DEFUN(symbol_to_keyword)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &o)) return FALSE;
  *result = gc_new_bytes_from(KEYWORD, o->bytes.elt, o->bytes.size);
  return TRUE;
}
