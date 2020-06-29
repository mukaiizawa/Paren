// array

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "splay.h"
#include "object.h"
#include "mem.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

// array

DEFUN(array_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, ARRAY));
  return TRUE;
}

DEFUN(array_new)
{
  int size;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_sint(argv->cons.car, &size)) return FALSE;
  *result = gc_new_array(size);
  return TRUE;
}

DEFUN(array_length)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, ARRAY, &o)) return FALSE;
  *result = gc_new_xint(o->array.size);
  return TRUE;
}

DEFUN(array_at)
{
  object o;
  int i;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_type(argv->cons.car, ARRAY, &o)) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &i)) return FALSE;
  if (i < 0 || i >= o->array.size) return FALSE;
  *result = o->array.elt[i];
  return TRUE;
}

DEFUN(array_put)
{
  object o;
  int i;
  if (!bi_argc_range(argc, 3, 3)) return FALSE;
  if (!bi_arg_type(argv->cons.car, ARRAY, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (i < 0 || i >= o->array.size) return FALSE;
  o->array.elt[i] = argv->cons.cdr->cons.car;
  *result = object_nil;
  return TRUE;
}
