// dictionary.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

DEFUN(dict)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  *result = gc_new_dict();
  return TRUE;
}

DEFUN(dict_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, DICT));
  return TRUE;
}

DEFUN(keys)
{
  int i;
  object o, *table;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  o = argv->cons.car;
  table = o->map.table;
  *result = object_nil;
  for (i = 0; i < o->map.half_size; i++)
    if (table[i] != NULL) *result = gc_new_cons(table[i], *result);
  return TRUE;
}

DEFUN(dict_access)
{
  object o, key;
  if (!bi_argc_range(argc, 2, 3)) return FALSE;
  if (!bi_arg_type(argv->cons.car, DICT, &o)) return FALSE;
  if (!bi_arg_immutable_mem((argv = argv->cons.cdr)->cons.car, &key)) return FALSE;
  if (argc == 2) {
    if ((*result = object_find(o, key)) == NULL) *result = object_nil;
  } else {
    *result = argv->cons.cdr->cons.car;
    object_bind(o, key, *result);
  }
  return TRUE;
}
