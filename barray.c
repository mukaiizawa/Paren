// byte-array

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "splay.h"
#include "object.h"
#include "mem.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

DEFUN(barray_new)
{
  int size;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_sint(argv->cons.car, &size)) return FALSE;
  *result = gc_new_barray(BARRAY, size);
  return TRUE;
}

DEFUN(barray_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, BARRAY));
  return TRUE;
}

DEFUN(barray_at)
{
  object o;
  int i;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_barray(argv->cons.car, &o)) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &i)) return FALSE;
  if (i < 0 || i >= o->barray.size) return FALSE;
  *result = sint(LC(o->barray.elt + i));
  return TRUE;
}

DEFUN(barray_put)
{
  object o;
  int i, byte;
  if (!bi_argc_range(argc, 3, 3)) return FALSE;
  if (!bi_arg_barray(argv->cons.car, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (i < 0 || i >= o->barray.size) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &byte)) return FALSE;
  if (!byte_p(byte)) return FALSE;
  SC(o->barray.elt + i, byte);
  *result = object_nil;
  return TRUE;
}

DEFUN(barray_length)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_barray(argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint(o->barray.size);
  return TRUE;
}

DEFUN(barray_copy)
{
  int oi, pi, size;
  object o, p;
  if (!bi_argc_range(argc, 5, 5)) return FALSE;
  if (!bi_arg_barray(argv->cons.car, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &oi)) return FALSE;
  if (!bi_arg_type((argv = argv->cons.cdr)->cons.car, BARRAY, &p)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &pi)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (size < 0) return FALSE;
  if (oi < 0 || (oi + size) > o->barray.size) return FALSE;
  if (pi < 0 || (pi + size) > p->barray.size) return FALSE;
  memmove(p->barray.elt + pi, o->barray.elt + oi, size);
  *result = p;
  return TRUE;
}

DEFUN(barray_index)
{
  object o, p;
  int b, s, e;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_arg_barray(argv->cons.car, &o)) return FALSE;
  p = NULL;
  switch (object_type((argv = argv->cons.cdr)->cons.car)) {
    case SINT:
      b = sint_val(argv->cons.car);
      if (!byte_p(b)) return FALSE;
      break;
    case SYMBOL:
    case KEYWORD:
    case BARRAY:
    case STRING:
      p = argv->cons.car;
      if (o->barray.size < p->barray.size) {
        *result = object_nil;
        return TRUE;
      }
      break;
    default:
      return FALSE;
  }
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &s)) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &e)) return FALSE;
  if (s > e) return FALSE;
  if (s < 0 || s > o->barray.size) return FALSE;
  if (e < 0 || e > o->barray.size) return FALSE;
  if (p == NULL) {
    while (s < e) {
      if (LC(o->barray.elt + s) == b) {
        *result = sint(s);
        return TRUE;
      }
      s++;
    }
  } else {
    while (s < e) {
      if (s + p->barray.size > o->barray.size) break;
      if (memcmp(o->barray.elt + s, p->barray.elt, p->barray.size) == 0) {
        *result = sint(s);
        return TRUE;
      }
      s++;
    }
  }
  *result = object_nil;
  return TRUE;
}

DEFUN(barray_unmatch_index)
{
  int i, oi, pi, size;
  object o, p;
  if (!bi_argc_range(argc, 5, 5)) return FALSE;
  if (!bi_arg_barray(argv->cons.car, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &oi)) return FALSE;
  if (!bi_arg_barray((argv = argv->cons.cdr)->cons.car, &p)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &pi)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (oi + size > o->barray.size) return FALSE;
  if (pi + size > p->barray.size) return FALSE;
  for(i = 0; i < size; i++) {
    if (LC(o->barray.elt + oi + i) != LC(p->barray.elt + pi + i)) break;
  }
  if (i == size) *result = object_nil;
  else *result = gc_new_xint(oi + i);
  return TRUE;
}

DEFUN(to_barray)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_barray(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray_from(BARRAY, o->barray.elt, o->barray.size);
  return TRUE;
}

DEFUN(barray_to_string)
{
  int i, size;
  object o;
  if (!bi_argc_range(argc, 3, 3)) return FALSE;
  if (!bi_arg_barray(argv->cons.car, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &size)) return FALSE;
  if (i < 0 || i + size > o->barray.size) return FALSE;
  *result = gc_new_barray_from(STRING, o->barray.elt + i, size);
  return TRUE;
}

DEFUN(barray_concat)
{
  struct xbarray x;
  object o, p;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  o = argv;
  xbarray_init(&x);
  while (o != object_nil) {
    if (!bi_arg_barray(o->cons.car, &p)) {
      xbarray_free(&x);
      return FALSE;
    }
    xbarray_copy(&x, p->barray.elt, p->barray.size);
    o = o->cons.cdr;
  }
  *result = gc_new_barray_from(object_type(argv->cons.car), x.elt, x.size);
  xbarray_free(&x);
  return TRUE;
}
