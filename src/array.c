// array.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

DEFUN(array)
{
  int size;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_sint(argv->cons.car, &size)) return FALSE;
  *result = gc_new_array(size);
  return TRUE;
}

DEFUN(array_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, ARRAY));
  return TRUE;
}
