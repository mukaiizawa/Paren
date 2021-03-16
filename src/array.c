// array

#include "std.h"
#include "object.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

DEFUN(array_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, ARRAY));
  return TRUE;
}

static int mem_at(object o, object argv, object *result)
{
  int i;
  if (!bi_sint(argv->cons.cdr->cons.car, &i)) return FALSE;
  if (i < 0 || i >= o->mem.size) return FALSE;
  *result = sint(LC(o->mem.elt + i));
  return TRUE;
}

static int array_at(object o, object argv, object *result)
{
  int i;
  if (!bi_sint(argv->cons.cdr->cons.car, &i)) return FALSE;
  if (i < 0 || i >= o->array.size) return FALSE;
  *result = o->array.elt[i];
  return TRUE;
}

static int mem_put(object o, object argv, object *result)
{
  int i, byte;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (i < 0 || i >= o->mem.size) return FALSE;
  if (!bi_sint((*result = argv->cons.cdr->cons.car), &byte)) return FALSE;
  if (!byte_p(byte)) return FALSE;
  SC(o->mem.elt + i, byte);
  return TRUE;
}

static int array_put(object o, object argv, object *result)
{
  int i;
  if (!bi_arg_type(argv->cons.car, ARRAY, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (i < 0 || i >= o->array.size) return FALSE;
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

DEFUN(array_copy)
{
  int oi, pi, size;
  object o, p;
  if (!bi_argc_range(argc, 5, 5)) return FALSE;
  if (!bi_arg_type(argv->cons.car, ARRAY, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &oi)) return FALSE;
  if (!bi_arg_type((argv = argv->cons.cdr)->cons.car, ARRAY, &p)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &pi)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (size < 0) return FALSE;
  if (oi < 0 || (oi + size) > o->array.size) return FALSE;
  if (pi < 0 || (pi + size) > p->array.size) return FALSE;
  memmove(p->array.elt + pi, o->array.elt + oi, sizeof(object) * size);
  *result = p;
  return TRUE;
}
