// keyword

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "splay.h"
#include "object.h"
#include "mem.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

DEFUN(keyword_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, KEYWORD));
  return TRUE;
}

DEFUN(keyword_to_symbol)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, KEYWORD, &o)) return FALSE;
  *result = gc_new_bytes_from(SYMBOL, o->bytes.elt, o->bytes.size);
  return TRUE;
}
