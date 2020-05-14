// sequence

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "mem.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

// cons

DEFUN(cons_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(type_p(argv->cons.car, CONS));
  return TRUE;
}

DEFUN(cons)
{
  object o;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_list(argv->cons.cdr->cons.car, &o)) return FALSE;
  *result = gc_new_cons(argv->cons.car, o);
  return TRUE;
}

DEFUN(list)
{
  *result = argv;
  return TRUE;
}

DEFUN(car)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.car;
  return TRUE;
}

DEFUN(set_car)
{
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_type(argv->cons.car, CONS, &o)) return FALSE;
  p = argv->cons.cdr->cons.car;
  o->cons.car = p;
  *result = p;
  return TRUE;
}

DEFUN(cdr)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  if (o == object_nil) *result = object_nil;
  else *result = o->cons.cdr;
  return TRUE;
}

DEFUN(set_cdr)
{
  object o, p;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_type(argv->cons.car, CONS, &o)) return FALSE;
  if (!bi_arg_list(argv->cons.cdr->cons.car, &p)) return FALSE;
  p = argv->cons.cdr->cons.car;
  o->cons.cdr = p;
  *result = p;
  return TRUE;
}

DEFUN(last_cons)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  if (o != object_nil) {
    while (o->cons.cdr != object_nil) o = o->cons.cdr;
  }
  *result = o;
  return TRUE;
}

DEFUN(assoc)
{
  object o, k;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  k = argv->cons.cdr->cons.car;
  while (o != object_nil) {
    if (o->cons.car == k) {
      if (!bi_arg_type(o->cons.cdr, CONS, &o)) break;
      *result = o->cons.car;
      return TRUE;
    }
    if (!bi_arg_type(o->cons.cdr, CONS, &o)) break;
    o = o->cons.cdr;
  }
  ip_mark_error("property not found");
  return FALSE;
}

DEFUN(set_assoc)
{
  object o, k, v;
  if (!bi_argc_range(argc, 3, 3)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
  k = argv->cons.cdr->cons.car;
  v = argv->cons.cdr->cons.cdr->cons.car;
  while (o != object_nil) {
    if (o->cons.car == k) {
      if (!bi_arg_type(o->cons.cdr, CONS, &o)) break;
      o->cons.car = v;
      *result = v;
      return TRUE;
    }
    if (!bi_arg_type(o->cons.cdr, CONS, &o)) break;
    o = o->cons.cdr;
  }
  ip_mark_error("property not found");
  return FALSE;
}

// barray

DEFUN(barray_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(type_p(argv->cons.car, BARRAY));
  return TRUE;
}

DEFUN(barray_new)
{
  int size;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_int(argv->cons.car, &size)) return FALSE;
  *result = gc_new_barray(BARRAY, size);
  return TRUE;
}

DEFUN(barray_copy)
{
  int fp, tp, size;
  object from, to;
  if (!bi_argc_range(argc, 5, 5)) return FALSE;
  from = argv->cons.car;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &fp)) {
    ip_mark_error("source array index must be integer");
    return FALSE;
  }
  to = (argv = argv->cons.cdr)->cons.car;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &tp)) {
    ip_mark_error("destination array index must be integer");
    return FALSE;
  }
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &size)) {
    ip_mark_error("copy size must be integer");
    return FALSE;
  }
  if (!type_p(to, BARRAY))
    ip_mark_error("destination must be byte-array");
  else if (fp < 0)
    ip_mark_error("source array index must be positive");
  else if (tp < 0)
    ip_mark_error("destination array index must be positive");
  else if (size <= 0)
    ip_mark_error("copy size must be positive");
  else if ((fp + size) > from->barray.size)
    ip_mark_error("source array index out of bounds exception");
  else if ((tp + size) > to->barray.size)
    ip_mark_error("destination array index out of bounds exception");
  else {
    memmove(to->barray.elt + tp, from->barray.elt + fp, size);
    *result = to;
    return TRUE;
  }
  return FALSE;
}

DEFUN(to_barray)
{
  object x;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_barray(argv->cons.car, &x)) return FALSE;
  if (type_p(x, BARRAY)) *result = x;
  else *result = gc_new_barray_from(BARRAY, x->barray.elt, x->barray.size);
  return TRUE;
}

DEFUN(barray_to_string)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, BARRAY, &o)) return FALSE;
  *result = gc_new_barray_from(STRING, o->barray.elt, o->barray.size);
  return TRUE;
}

// array

DEFUN(array_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(type_p(argv->cons.car, ARRAY));
  return TRUE;
}

DEFUN(array_new)
{
  int size;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_int(argv->cons.car, &size)) return FALSE;
  *result = gc_new_array(size);
  return TRUE;
}

// string

DEFUN(string_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(type_p(argv->cons.car, STRING));
  return TRUE;
}

DEFUN(string_to_symbol)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, STRING, &o)) return FALSE;
  *result = gc_new_barray_from(SYMBOL, o->barray.elt, o->barray.size);
  return TRUE;
}

// symbol

DEFUN(symbol_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(type_p(argv->cons.car, SYMBOL));
  return TRUE;
}

DEFUN(symbol_to_string)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &o)) return FALSE;
  *result = gc_new_barray_from(STRING, o->barray.elt, o->barray.size);
  return TRUE;
}

DEFUN(symbol_to_keyword)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &o)) return FALSE;
  *result = gc_new_barray_from(KEYWORD, o->barray.elt, o->barray.size);
  return TRUE;
}

// keyword

DEFUN(keyword_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(type_p(argv->cons.car, KEYWORD));
  return TRUE;
}

DEFUN(keyword_to_symbol)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, KEYWORD, &o)) return FALSE;
  *result = gc_new_barray_from(SYMBOL, o->barray.elt, o->barray.size);
  return TRUE;
}

// sequence

static int char_trail_p(unsigned char b)
{
  return (b & 0xC0) == 0x80;
}

static int char_size0(object o, int start, int *result)
{
  int i;
  unsigned char b1, b2, b3, b4;
  i = start;
  if (i >= o->barray.size) return FALSE;
  b1 = (unsigned char)o->barray.elt[i];
  *result = 1;
  if (b1 < 0) return FALSE;
  if (b1 < 0x80) return TRUE;
  if (b1 < 0xC2) return FALSE;
  if (++i >= o->barray.size) return FALSE;
  b2 = (unsigned char)o->barray.elt[i];
  if (!char_trail_p(b2)) return FALSE;
  *result = 2;
  if (b1 < 0xE0) return !((b1 & 0x3E) == 0);
  if (++i >= o->barray.size) return FALSE;
  b3 = (unsigned char)o->barray.elt[i];
  if (!char_trail_p(b3)) return FALSE;
  *result = 3;
  if (b1 < 0xF0) return !((b1 == 0xE0) && ((b2 & 0x20) == 0));
  if (++i >= o->barray.size) return FALSE;
  b4 = (unsigned char)o->barray.elt[i];
  if (!char_trail_p(b4)) return FALSE;
  *result = 4;
  if (b1 < 0xF8) return !((b1 == 0xF0) && ((b2 & 0x30) == 0));
  return FALSE;
}

static int char_size(object o, int start, int *result)
{
  if (char_size0(o, start, result)) return TRUE;
  ip_mark_error("illegal utf8 string");
  return FALSE;
}

static int string_nth_index(object o, int n, int *i, int *size)
{
  *i = 0;
  while (TRUE) {
    if (!char_size(o, *i, size)) return FALSE;
    if (n == 0) return TRUE;
    *i += *size;
    n--;
  }
  return TRUE;
}

DEFUN(barray_unmatch_index)
{
  int i, oi, pi, size;
  object o, p;
  if (!bi_argc_range(argc, 5, 5)) return FALSE;
  if (!bi_arg_barray(argv->cons.car, &o)) return FALSE;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &oi)) return FALSE;
  if (!bi_arg_barray((argv = argv->cons.cdr)->cons.car, &p)) return FALSE;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &pi)) return FALSE;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if(oi + size > o->barray.size) return FALSE;
  if(pi + size > o->barray.size) return FALSE;
  for(i = 0; i < size; i++) {
    if(LC(o->barray.elt + oi + i) != LC(o->barray.elt + pi + i)) break;
  }
  if (i == size) *result = object_nil;
  else *result = gc_new_xint(oi + i);
  return TRUE;
}

static int string_length(object o, int *len)
{
  int i, csize;
  i = *len = 0;
  while (i < o->barray.size) {
    if (!char_size(o, i, &csize)) return FALSE;
    (*len)++;
    i += csize;
  }
  return TRUE;
}

static int seq_length(object o, int *len)
{
  switch (type(o)) {
    case CONS:
      *len = object_list_len(o);
      return TRUE;
    case ARRAY:
      *len = o->array.size;
      return TRUE;
    case SYMBOL:
    case KEYWORD:
    case BARRAY:
      if (o == object_nil) *len = 0;
      else *len = o->barray.size;
      return TRUE;
    case STRING:
      return string_length(o, len);
    default:
      return FALSE;
  }
}

DEFUN(length)
{
  int len;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!seq_length(argv->cons.car, &len)) return FALSE;
  *result = gc_new_xint(len);
  return TRUE;
}

static int cons_subseq(object o, int start, int end, object *result)
{
  int i;
  object acc;
  acc = object_nil;
  for (i = 0; i < end; i++, o = o->cons.cdr) {
    if (i < start) continue;
    acc = gc_new_cons(o->cons.car, acc);
  }
  *result = object_reverse(acc);
  return TRUE;
}

static int string_subseq(object o, int start, int end, object *result)
{
  int i, n, copy_s, copy_e, len;
  i = n = copy_s = 0;
  while (TRUE) {
    if (n == start) copy_s = i;
    if (n == end) {
      copy_e = i;
      break;
    }
    if (!char_size(o, i, &len)) return FALSE;
    i += len;
    n++;
  }
  *result = gc_new_barray_from(STRING, o->barray.elt + copy_s, copy_e - copy_s);
  return TRUE;
}

DEFUN(subseq)
{
  int start, end, len;
  object o, s, e;
  if (!bi_argc_range(argc, 2, 3)) return FALSE;
  if (!seq_length(o = argv->cons.car, &len)) return FALSE;
  if (!bi_arg_type((argv = argv->cons.cdr)->cons.car, XINT, &s)) return FALSE;
  start = s->xint.val;
  if (argc == 2) end = len;
  else {
    if (!bi_arg_type(argv->cons.cdr->cons.car, XINT, &e)) return FALSE;
    end = e->xint.val;
    if (end > len) return FALSE;
  }
  if (start > end) return FALSE;
  switch (type(o)) {
    case CONS:
      return cons_subseq(o, start, end, result);
    case ARRAY:
      *result = gc_new_array_from(o->array.elt + start, end - start);
      return TRUE;
    case BARRAY:
      *result = gc_new_barray_from(BARRAY, o->barray.elt + start, end - start);
      return TRUE;
    case STRING:
      return string_subseq(o, start, end, result);
    default:
      return FALSE;
  }
}

static int concat_symbol(object argv, object *result)
{
  struct xbarray x;
  object o;
  xbarray_init(&x);
  while (argv != object_nil) {
    if (!bi_arg_type(argv->cons.car, SYMBOL, &o)) return FALSE;
    xbarray_copy(&x, o->barray.elt, o->barray.size);
    argv = argv->cons.cdr;
  }
  *result = gc_new_barray_from(SYMBOL, x.elt, x.size);
  xbarray_free(&x);
  return TRUE;
}

static int concat_cons(object argv, object *result)
{
  object o, acc;
  acc = object_nil;
  while (argv != object_nil) {
    if (!bi_arg_list(argv->cons.car, &o)) return FALSE;
    while (o != object_nil) {
      acc = gc_new_cons(o->cons.car, acc);
      o = o->cons.cdr;
    }
    argv = argv->cons.cdr;
  }
  *result = object_reverse(acc);
  return TRUE;
}

static int concat_barray(int type, object argv, object *result)
{
  int psize;
  object o, p;
  *result = argv->cons.car;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!bi_arg_type(argv->cons.car, type, &o)) return FALSE;
    p = *result;
    psize = p->barray.size;
    *result = gc_new_barray(type, psize + o->barray.size);
    memcpy((*result)->barray.elt, p->barray.elt, psize);
    memcpy((*result)->barray.elt + psize, o->barray.elt, o->barray.size);
  }
  return TRUE;
}

static int concat_array(object argv, object *result)
{
  int osize, psize;
  object o, p;
  *result = argv->cons.car;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!bi_arg_type(argv->cons.car, ARRAY, &o)) return FALSE;
    p = *result;
    psize = sizeof(object) * p->barray.size;
    osize = sizeof(object) * o->barray.size;
    *result = gc_new_barray(ARRAY, psize + osize);
    memcpy((*result)->barray.elt, p->barray.elt, psize);
    memcpy((*result)->barray.elt + psize, o->barray.elt, osize);
  }
  return TRUE;
}

DEFUN(concat)
{
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  switch (type(argv->cons.car)) {
    case SYMBOL:
      if (argv->cons.car != object_nil)
        return concat_symbol(argv, result);
    case CONS:
      return concat_cons(argv, result);
    case ARRAY:
      return concat_array(argv, result);
    case BARRAY:
      return concat_barray(BARRAY, argv, result);
    case STRING:
      return concat_barray(STRING, argv, result);
    default:
      return FALSE;
  }
}

static int nth_string(object o, int n, object *result)
{
  int start, size;
  if (!string_nth_index(o, n, &start, &size)) return FALSE;
  *result = gc_new_barray_from(STRING, o->barray.elt + start, size);
  return TRUE;
}

DEFUN(nth)
{
  object o;
  int i, len;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  o = argv->cons.car;
  if (!bi_int(argv->cons.cdr->cons.car, &i)) return FALSE;
  if (i < 0) return FALSE;
  if (list_p(o)) {
    while (o != object_nil) {
      if (i-- == 0) {
        o = o->cons.car;
        break;
      }
      o = o->cons.cdr;
    }
    *result = o;
    return TRUE;
  }
  if (!seq_length(o = argv->cons.car, &len)) return FALSE;
  if (i >= len) return FALSE;
  switch (type(o)) {
    case ARRAY:
      *result = o->array.elt[i];
      return TRUE;
    case BARRAY:
      *result = gc_new_xint((unsigned char)o->barray.elt[i]);
      return TRUE;
    case STRING:
      return nth_string(o, i, result);
    default:
      return FALSE;
  }
}

static int nth_set_string(object o, int n, object v, object *result)
{
  int start, size;
  if (!string_nth_index(o, n, &start, &size)) return FALSE;
  *result = gc_new_barray(STRING, o->barray.size + v->barray.size - size);
  if (start != 0)
    memcpy((*result)->barray.elt, o->barray.elt, start);
  memcpy((*result)->barray.elt + start, v->barray.elt, v->barray.size);
  if ((*result)->barray.size > start + v->barray.size)
    memcpy((*result)->barray.elt + start + v->barray.size
        , o->barray.elt + start + size
        , (*result)->barray.size - start - v->barray.size);
  return TRUE;
}

DEFUN(nth_set)
{
  object o, v;
  int i, len;
  if (!bi_argc_range(argc, 3, 3)) return FALSE;
  if (!seq_length(o = argv->cons.car, &len)) return FALSE;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  if (i < 0 || i >= len) return FALSE;
  v = argv->cons.cdr->cons.car;
  switch (type(o)) {
    case CONS:
      while (i--) o = o->cons.cdr;
      *result = v;
      o->cons.car = v;
      return TRUE;
    case ARRAY:
      *result = v;
      o->array.elt[i] = v;
      return TRUE;
    case BARRAY:
      if (!byte_p(v)) return FALSE;
      *result = v;
      o->barray.elt[i] = (char)v->xint.val;
      return TRUE;
    case STRING:
      if (!type_p(v, STRING)) return FALSE;
      if (!nth_set_string(o, i, v, result)) return FALSE;
      return TRUE;
    default: break;
  }
  return FALSE;
}

DEFUN(reverse)
{
  int i, j, n, size, csize;
  object o, p;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  switch (type(o = argv->cons.car)) {
    case SYMBOL:
      if (o != object_nil) break;
    case CONS:
      p = object_nil;
      while (o != object_nil) {
        p = gc_new_cons(o->cons.car, p);
        o = o->cons.cdr;
      }
      *result = p;
      return TRUE;
    case ARRAY:
      size = o->array.size;
      p = gc_new_array(size);
      for (i = 0; i < size; i++) p->array.elt[i] = o->array.elt[size - i - 1];
      *result = p;
      return TRUE;
    case BARRAY:
      size = o->barray.size;
      p = gc_new_barray(BARRAY, size);
      for (i = 0; i < size; i++) p->barray.elt[i] = o->barray.elt[size - i - 1];
      *result = p;
      return TRUE;
    case STRING:
      p = gc_new_barray(STRING, o->barray.size);
      if (!string_length(o, &size)) return FALSE;
      for (i = 0, n = 0; n < size; n++) {
        if (!string_nth_index(o, size - n, &j, &csize)) return FALSE;
        memcpy(p->barray.elt + i, o->barray.elt + j, csize);
        i += csize;
      }
      *result = p;
      return TRUE;
    default: break;
  }
  return FALSE;
}

DEFUN(xreverse)
{
  int i, size;
  unsigned char c;
  object o, p;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  switch (type(o = argv->cons.car)) {
    case SYMBOL:
      if (argv->cons.car != object_nil) break;
    case CONS:
      *result = object_reverse(o);
      return TRUE;
    case ARRAY:
      size = o->array.size;
      for (i = 0; i < size / 2; i++) {
        p = o->array.elt[i];
        o->array.elt[i] = o->array.elt[size - i - 1];
        o->array.elt[size - i - 1] = p;
      }
      *result = o;
      return TRUE;
    case BARRAY:
      size = o->barray.size;
      for (i = 0; i < size / 2; i++) {
        c = (unsigned char)o->barray.elt[i];
        o->barray.elt[i] = o->barray.elt[size - i - 1];
        o->barray.elt[size - i - 1] = (char)c;
      }
      *result = o;
      return TRUE;
    default: break;
  }
  return FALSE;
}
