// bytes

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "splay.h"
#include "object.h"
#include "mem.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

DEFUN(bytes_new)
{
  int size;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_sint(argv->cons.car, &size)) return FALSE;
  *result = gc_new_bytes(BYTES, size);
  return TRUE;
}

static int bytes_p(int argc, object argv, object *result, int type)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, type));
  return TRUE;
}

DEFUN(symbol_p)
{
  return bytes_p(argc, argv, result, SYMBOL);
}

DEFUN(keyword_p)
{
  return bytes_p(argc, argv, result, KEYWORD);
}

DEFUN(string_p)
{
  return bytes_p(argc, argv, result, STRING);
}

DEFUN(bytes_p)
{
  return bytes_p(argc, argv, result, BYTES);
}

static int bytes_to(int argc, object argv, object *result, int type)
{
  int i, size;
  object o;
  if (!bi_argc_range(argc, 1, 3)) return FALSE;
  if (!bi_arg_bytes(argv->cons.car, &o)) return FALSE;
  if (argc < 2) i = 0;
  else if (!bi_sint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (argc < 3) size = o->bytes.size;
  else if (!bi_sint(argv->cons.cdr->cons.car, &size)) return FALSE;
  if (i < 0 || i + size > o->bytes.size) return FALSE;
  *result = gc_new_bytes_from(type, o->bytes.elt + i, size);
  return TRUE;
}

DEFUN(to_bytes)
{
  return bytes_to(argc, argv, result, BYTES);
}

DEFUN(bytes_to_symbol)
{
  return bytes_to(argc, argv, result, SYMBOL);
}

DEFUN(bytes_to_keyword)
{
  return bytes_to(argc, argv, result, KEYWORD);
}

DEFUN(bytes_to_string)
{
  return bytes_to(argc, argv, result, STRING);
}

DEFUN(bytes_at)
{
  object o;
  int i;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_bytes(argv->cons.car, &o)) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &i)) return FALSE;
  if (i < 0 || i >= o->bytes.size) return FALSE;
  *result = sint(LC(o->bytes.elt + i));
  return TRUE;
}

DEFUN(bytes_put)
{
  object o;
  int i, byte;
  if (!bi_argc_range(argc, 3, 3)) return FALSE;
  if (!bi_arg_bytes(argv->cons.car, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (i < 0 || i >= o->bytes.size) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &byte)) return FALSE;
  if (!byte_p(byte)) return FALSE;
  SC(o->bytes.elt + i, byte);
  *result = object_nil;
  return TRUE;
}

DEFUN(bytes_length)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_bytes(argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint(o->bytes.size);
  return TRUE;
}

DEFUN(bytes_copy)
{
  int oi, pi, size;
  object o, p;
  if (!bi_argc_range(argc, 5, 5)) return FALSE;
  if (!bi_arg_bytes(argv->cons.car, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &oi)) return FALSE;
  if (!bi_arg_type((argv = argv->cons.cdr)->cons.car, BYTES, &p)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &pi)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (size < 0) return FALSE;
  if (oi < 0 || (oi + size) > o->bytes.size) return FALSE;
  if (pi < 0 || (pi + size) > p->bytes.size) return FALSE;
  memmove(p->bytes.elt + pi, o->bytes.elt + oi, size);
  *result = p;
  return TRUE;
}

DEFUN(bytes_index)
{
  object o, p;
  int b, s, e;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_arg_bytes(argv->cons.car, &o)) return FALSE;
  p = NULL;
  switch (object_type((argv = argv->cons.cdr)->cons.car)) {
    case SINT:
      b = sint_val(argv->cons.car);
      if (!byte_p(b)) return FALSE;
      break;
    case SYMBOL:
    case KEYWORD:
    case BYTES:
    case STRING:
      p = argv->cons.car;
      if (o->bytes.size < p->bytes.size) {
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
  if (s < 0 || s > o->bytes.size) return FALSE;
  if (e < 0 || e > o->bytes.size) return FALSE;
  if (p == NULL) {
    while (s < e) {
      if (LC(o->bytes.elt + s) == b) {
        *result = sint(s);
        return TRUE;
      }
      s++;
    }
  } else {
    while (s < e) {
      if (s + p->bytes.size > o->bytes.size) break;
      if (memcmp(o->bytes.elt + s, p->bytes.elt, p->bytes.size) == 0) {
        *result = sint(s);
        return TRUE;
      }
      s++;
    }
  }
  *result = object_nil;
  return TRUE;
}

DEFUN(bytes_unmatch_index)
{
  int i, oi, pi, size;
  object o, p;
  if (!bi_argc_range(argc, 5, 5)) return FALSE;
  if (!bi_arg_bytes(argv->cons.car, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &oi)) return FALSE;
  if (!bi_arg_bytes((argv = argv->cons.cdr)->cons.car, &p)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &pi)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (oi + size > o->bytes.size) return FALSE;
  if (pi + size > p->bytes.size) return FALSE;
  for(i = 0; i < size; i++) {
    if (LC(o->bytes.elt + oi + i) != LC(p->bytes.elt + pi + i)) break;
  }
  if (i == size) *result = object_nil;
  else *result = gc_new_xint(oi + i);
  return TRUE;
}

DEFUN(bytes_concat)
{
  struct xbarray x;
  object o, p;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  o = argv;
  xbarray_init(&x);
  while (o != object_nil) {
    if (!bi_arg_bytes(o->cons.car, &p)) {
      xbarray_free(&x);
      return FALSE;
    }
    xbarray_copy(&x, p->bytes.elt, p->bytes.size);
    o = o->cons.cdr;
  }
  *result = gc_new_bytes_from(object_type(argv->cons.car), x.elt, x.size);
  xbarray_free(&x);
  return TRUE;
}
