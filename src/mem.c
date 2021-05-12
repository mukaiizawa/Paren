// memory.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

static int mem_p(int argc, object argv, object *result, int type)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(object_type_p(argv->cons.car, type));
  return TRUE;
}

DEFUN(symbol_p)
{
  return mem_p(argc, argv, result, SYMBOL);
}

DEFUN(keyword_p)
{
  return mem_p(argc, argv, result, KEYWORD);
}

DEFUN(string_p)
{
  return mem_p(argc, argv, result, STRING);
}

DEFUN(bytes_p)
{
  return mem_p(argc, argv, result, BYTES);
}

static int mem_to(int argc, object argv, object *result, int type)
{
  int i, size;
  object o;
  if (!bi_argc_range(argc, 1, 3)) return FALSE;
  if (!bi_arg_mem(argv->cons.car, &o)) return FALSE;
  if (argc < 2) i = 0;
  else if (!bi_spint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (argc < 3) size = o->mem.size - i;
  else if (!bi_sint(argv->cons.cdr->cons.car, &size)) return FALSE;
  if (i + size > o->mem.size) return FALSE;
  *result = gc_new_mem_from(type, o->mem.elt + i, size);
  return TRUE;
}

DEFUN(bytes)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (object_type_p(argv->cons.car, SINT)) {
    *result = gc_new_mem(BYTES, sint_val(argv->cons.car));
    return TRUE;
  }
  return mem_to(argc, argv, result, BYTES);
}

DEFUN(symbol)
{
  static int c = 0;
  if (argc == 0) {
    xbarray_reset(&bi_buf);
    xbarray_addf(&bi_buf, "$G-%d", ++c);
    *result = gc_new_mem_from(SYMBOL, bi_buf.elt, bi_buf.size);
    return TRUE;
  }
  return mem_to(argc, argv, result, SYMBOL);
}

DEFUN(keyword)
{
  return mem_to(argc, argv, result, KEYWORD);
}

DEFUN(string)
{
  return mem_to(argc, argv, result, STRING);
}

DEFUN(xstring)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_mutable_mem(argv->cons.car, &o)) return FALSE;
  object_set_type(o, STRING);
  *result = o;
  return TRUE;
}

DEFUN(memcpy)
{
  int oi, pi, size;
  object o, p;
  if (!bi_argc_range(argc, 5, 5)) return FALSE;
  if (!bi_arg_mem(argv->cons.car, &o)) return FALSE;
  if (!bi_spint((argv = argv->cons.cdr)->cons.car, &oi)) return FALSE;
  if (!bi_arg_mutable_mem((argv = argv->cons.cdr)->cons.car, &p)) return FALSE;
  if (!bi_spint((argv = argv->cons.cdr)->cons.car, &pi)) return FALSE;
  if (!bi_spint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if ((oi + size) > o->mem.size) return FALSE;
  if ((pi + size) > p->mem.size) return FALSE;
  memmove(p->mem.elt + pi, o->mem.elt + oi, size);
  *result = p;
  return TRUE;
}

DEFUN(memmem)
{
  int b, s, e;
  object o, p;
  if (!bi_argc_range(argc, 2, 4)) return FALSE;
  if (!bi_arg_mem(argv->cons.car, &o)) return FALSE;
  b = 0;
  p = NULL;
  argv = argv->cons.cdr;
  switch (object_type(argv->cons.car)) {
    case SINT:
      b = sint_val(argv->cons.car);
      if (!byte_p(b)) return FALSE;
      break;
    case SYMBOL:
    case KEYWORD:
    case BYTES:
    case STRING:
      p = argv->cons.car;
      if (o->mem.size < p->mem.size) {
        *result = object_nil;
        return TRUE;
      }
      break;
    default:
      return FALSE;
  }
  if (argc < 3) s = 0;
  else if (!bi_spint((argv = argv->cons.cdr)->cons.car, &s)) return FALSE;
  if (argc < 4) e = o->mem.size;
  else if (!bi_spint(argv->cons.cdr->cons.car, &e)) return FALSE;
  if (s > e || s > o->mem.size || e > o->mem.size) return FALSE;
  if (p == NULL) {
    while (s < e) {
      if (LC(o->mem.elt + s) == b) {
        *result = sint(s);
        return TRUE;
      }
      s++;
    }
  } else {
    while (s < e) {
      if (s + p->mem.size > o->mem.size) break;
      if (memcmp(o->mem.elt + s, p->mem.elt, p->mem.size) == 0) {
        *result = sint(s);
        return TRUE;
      }
      s++;
    }
  }
  *result = object_nil;
  return TRUE;
}

DEFUN(mem_eq_p)
{
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_mem(argv->cons.car, &o)) return FALSE;
  if (!bi_arg_mem(argv->cons.cdr->cons.car, &p)) return FALSE;
  *result = object_bool((o->mem.size == p->mem.size)
      && memcmp(o->mem.elt, p->mem.elt, p->mem.size) == 0);
  return TRUE;
}

DEFUN(memlen)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_mem(argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint(o->mem.size);
  return TRUE;
}

DEFUN(memcmp)
{
  int val, size;
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_mem(argv->cons.car, &o)) return FALSE;
  if (!bi_arg_mem(argv->cons.cdr->cons.car, &p)) return FALSE;
  size = o->mem.size;
  if (size > p->mem.size) size = p->mem.size;
  if ((val = memcmp(o->mem.elt, p->mem.elt, size)) == 0) {
    if (o->mem.size < p->mem.size) val = -1;
    else if (o->mem.size > p->mem.size) val = 1;
  }
  *result = gc_new_xint(val);
  return TRUE;
}

DEFUN(memcat)
{
  struct xbarray x;
  object o, p;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  o = argv;
  xbarray_init(&x);
  while (o != object_nil) {
    if (!bi_arg_mem(o->cons.car, &p)) {
      xbarray_free(&x);
      return FALSE;
    }
    xbarray_copy(&x, p->mem.elt, p->mem.size);
    o = o->cons.cdr;
  }
  *result = gc_new_mem_from(object_type(argv->cons.car), x.elt, x.size);
  xbarray_free(&x);
  return TRUE;
}

DEFUN(chr)
{
  int x, size;
  char buf[4];
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_spint(argv->cons.car, &x)) return FALSE;
  if (x <= 0x7f) {
    size = 1;
    buf[0] = x;
  } else if (x <= 0x7ff) {
    size = 2;
    buf[0] = 0xc0 | ((x >> 6) & 0x3f);
    buf[1] = 0x80 | (x & 0x3f);
  } else if (x <= 0xffff) {
    size = 3;
    buf[0] = 0xe0 | ((x >> 12) & 0x3f);
    buf[1] = 0x80 | ((x >> 6) & 0x3f);
    buf[2] = 0x80 | (x & 0x3f);
  } else if (x <= 0xfffff) {
    size = 4;
    buf[0] = 0xf0 | ((x >> 18) & 0x3f);
    buf[1] = 0x80 | ((x >> 12) & 0x3f);
    buf[2] = 0x80 | ((x >> 6) & 0x3f);
    buf[3] = 0x80 | (x & 0x3f);
  } else return FALSE;
  *result = gc_new_mem_from(STRING, buf, size);
  return TRUE;
}

DEFUN(ord)
{
  int x;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, STRING, &o)) return FALSE;
  switch (o->mem.size) {
    case 1:
      x = LC(o->mem.elt);
      break;
    case 2:
      x = ((LC(o->mem.elt) & 0x3f) << 6)
        | (LC(o->mem.elt + 1) & 0x3f);
      break;
    case 3:
      x = ((LC(o->mem.elt) & 0xf) << 12)
        | ((LC(o->mem.elt + 1) & 0x3f) << 6)
        | (LC(o->mem.elt + 2) & 0x3f);
      break;
    case 4:
      x = ((LC(o->mem.elt) & 0x3) << 18)
        | ((LC(o->mem.elt + 1) & 0x3f) << 12)
        | ((LC(o->mem.elt + 2) & 0x3f) << 6)
        | (LC(o->mem.elt + 3) & 0x3f);
      break;
    default:
      return FALSE;
  }
  *result = gc_new_xint(x);
  return TRUE;
}

DEFUN(ascii_p)
{
  int len;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, STRING, &o)) return FALSE;
  if (!str_len(o, &len)) return FALSE;
  *result = object_bool(len == o->mem.size);
  return TRUE;
}

static int xctype_p(int argc, object argv, int (*f)(int c), object *result)
{
  int i;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, STRING, &o)) return FALSE;
  *result = object_nil;
  if (o->mem.size == 0) return TRUE;
  i = 0;
  while (i < o->mem.size) {
    if (!f(LC(o->mem.elt + i))) return TRUE;
    if (!(ch_len(LC(o->mem.elt + i), &i))) return FALSE;
  }
  *result = object_true;
  return TRUE;
}

DEFUN(alnum_p)
{
  return xctype_p(argc, argv, isalnum, result);
}

DEFUN(alpha_p)
{
  return xctype_p(argc, argv, isalpha, result);
}

DEFUN(digit_p)
{
  return xctype_p(argc, argv, isdigit, result);
}

DEFUN(space_p)
{
  return xctype_p(argc, argv, isspace, result);
}

DEFUN(print_p)
{
  return xctype_p(argc, argv, isprint, result);
}

DEFUN(lower_p)
{
  return xctype_p(argc, argv, islower, result);
}

DEFUN(upper_p)
{
  return xctype_p(argc, argv, isupper, result);
}

static int ch_conv_case(int argc, object argv, int (*f)(int c), int offset, object *result)
{
  int i, ch;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, STRING, &o)) return FALSE;
  o = gc_new_mem_from(STRING, o->mem.elt, o->mem.size);
  i = 0;
  while (i < o->mem.size) {
    if (f(ch = LC(o->mem.elt + i))) SC(o->mem.elt + i, ch + offset);
    if (!(ch_len(ch, &i))) return FALSE;
  }
  *result = o;
  return TRUE;
}

DEFUN(lower)
{
  return ch_conv_case(argc, argv, isupper, 0x20, result);
}

DEFUN(upper)
{
  return ch_conv_case(argc, argv, islower, -0x20, result);
}
