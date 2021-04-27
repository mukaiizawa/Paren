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

static int mem_at(object o, object argv, object *result)
{
  int i;
  if (!bi_spint(argv->cons.cdr->cons.car, &i)) return FALSE;
  if (i >= o->mem.size) return FALSE;
  *result = sint(LC(o->mem.elt + i));
  return TRUE;
}

static int array_at(object o, object argv, object *result)
{
  int i;
  if (!bi_spint(argv->cons.cdr->cons.car, &i)) return FALSE;
  if (i >= o->array.size) return FALSE;
  *result = o->array.elt[i];
  return TRUE;
}

static int mem_put(object o, object argv, object *result)
{
  int i, byte;
  if (!bi_spint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (i >= o->mem.size) return FALSE;
  if (!bi_sint((*result = argv->cons.cdr->cons.car), &byte)) return FALSE;
  if (!byte_p(byte)) return FALSE;
  SC(o->mem.elt + i, byte);
  return TRUE;
}

static int array_put(object o, object argv, object *result)
{
  int i;
  if (!bi_arg_type(argv->cons.car, ARRAY, &o)) return FALSE;
  if (!bi_spint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (i >= o->array.size) return FALSE;
  *result = o->array.elt[i] = argv->cons.cdr->cons.car;
  return TRUE;
}

DEFUN(array_access)
{
  object o;
  if (!bi_argc_range(argc, 2, 3)) return FALSE;
  o = argv->cons.car;
  switch (object_type(o)) {
    case BYTES:
    case STRING:
    case SYMBOL:
    case KEYWORD:
      if (argc == 2) return mem_at(o, argv, result);
      return mem_put(o, argv, result);
    case ARRAY:
      if (argc == 2) return array_at(o, argv, result);
      return array_put(o, argv, result);
    default:
      return FALSE;
  }
}
