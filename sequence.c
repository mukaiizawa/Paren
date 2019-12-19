// sequence

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int ensure_symbol(object o, object *result)
{
  if (!typep(o, SYMBOL)) {
    ip_mark_exception("requires symbol");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

static int ensure_keyword(object o, object *result)
{
  if (!typep(o, KEYWORD)) {
    ip_mark_exception("requires keyword");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

static int ensure_string(object o, object *result)
{
  if (!typep(o, STRING)) {
    ip_mark_exception("requires string");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

static int ensure_byte_array(object o, object *result)
{
  if (!typep(o, BARRAY)) {
    ip_mark_exception("requires byte-array");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

static int ensure_array(object o, object *result)
{
  if (!typep(o, ARRAY)) {
    ip_mark_exception("requires array");
    return FALSE;
  }
  *result = o;
  return TRUE;
}

static int sequence_access(object seq, object rest, object *result)
{
  int i;
  object v;
  if (!bi_int(rest->cons.car, &i)) return FALSE;
  if (i < 0 || i >= seq->barray.size) return FALSE;
  if (rest->cons.cdr == object_nil)
    *result = gc_new_xint((unsigned char)(seq->barray.elt[i]));
  else {
    v = rest->cons.cdr->cons.car;
    if (!bytep(v)) return FALSE;
    seq->barray.elt[i] = (char)(v->xint.val);
    *result = v;
  }
  return TRUE;
}

// array

PRIM(array_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, ARRAY));
  return TRUE;
}

PRIM(array_length)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_array(argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint(argv->cons.car->array.size);
  return TRUE;
}

// barray

PRIM(barray_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, BARRAY));
  return TRUE;
}

PRIM(barray_new)
{
  int size;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!bi_int(argv->cons.car, &size)) return FALSE;
  *result = gc_new_barray(BARRAY, size);
  return TRUE;
}

PRIM(barray_access)
{
  object a;
  if (!ip_ensure_arguments(argc, 2, 3)) return FALSE;
  if (!ensure_byte_array(argv->cons.car, &a)) return FALSE;
  return sequence_access(a, argv->cons.cdr, result);
}

PRIM(barray_length)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_byte_array(argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint(argv->cons.car->array.size);
  return TRUE;
}

PRIM(barray_copy)
{
  int fp, tp, size;
  object from, to;
  if (!ip_ensure_arguments(argc, 5, 5)) return FALSE;
  from = argv->cons.car;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &fp)) {
    ip_mark_exception("source array index must be integer");
    return FALSE;
  }
  to = (argv = argv->cons.cdr)->cons.car;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &tp)) {
    ip_mark_exception("destination array index must be integer");
    return FALSE;
  }
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &size)) {
    ip_mark_exception("copy size must be integer");
    return FALSE;
  }
  if (!typep(to, BARRAY))
    ip_mark_exception("destination must be byte-array");
  else if (fp < 0)
    ip_mark_exception("source array index must be positive");
  else if (tp < 0)
    ip_mark_exception("destination array index must be positive");
  else if (size <= 0)
    ip_mark_exception("copy size must be positive");
  else if ((fp + size) > from->barray.size)
    ip_mark_exception("source array index out of bounds exception");
  else if ((tp + size) > to->barray.size)
    ip_mark_exception("destination array index out of bounds exception");
  else {
    memmove(to->barray.elt + tp, from->barray.elt + fp, size);
    *result = to;
    return TRUE;
  }
  return FALSE;
}

PRIM(barray_index)
{
  int i, olen, plen;
  object o, p;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!ensure_byte_array(argv->cons.car, &o)) return FALSE;
  if (!ensure_byte_array(argv->cons.cdr->cons.car, &p)) return FALSE;
  olen = o->barray.size;
  plen = p->barray.size;
  *result = object_nil;
  i = 0;
  while (i + plen <= olen) {
    if (memcmp(o->barray.elt + i, p->barray.elt, plen) == 0) {
      *result = gc_new_xint(i);
      break;
    }
    i++;
  }
  return TRUE;
}

PRIM(barray_to_string)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_byte_array(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray_from(STRING, o->barray.size, o->barray.elt);
  return TRUE;
}

PRIM(barray_slice)
{
  int s, e;
  object o;
  if (!ip_ensure_arguments(argc, 2, FALSE)) return FALSE;
  if (!ensure_byte_array(argv->cons.car, &o)) return FALSE;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &s)) return FALSE;
  if (argv->cons.cdr == object_nil) e = o->barray.size;
  else if (!bi_int((argv = argv->cons.cdr)->cons.car, &e)) return FALSE;
  if (s < 0 || s >= e || e > o->barray.size) return FALSE;
  *result = gc_new_barray_from(BARRAY, e - s, o->barray.elt + s);
  return TRUE;
}

// string

PRIM(string_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, STRING));
  return TRUE;
}

PRIM(string_add)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, FALSE)) return FALSE;
  if (!ensure_string(argv->cons.car, result)) return FALSE;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!ensure_string(argv->cons.car, &x)) return FALSE;
    barray_add(STRING, x, result);
  }
  return TRUE;
}

PRIM(string_equal)
{
  int b;
  object x, y;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  if (!ensure_string(argv->cons.cdr->cons.car, &y)) return FALSE;
  if (x->barray.size != y->barray.size) b = FALSE;
  else b = memcmp(x->barray.elt, y->barray.elt, x->barray.size) == 0;
  *result = object_bool(b);
  return TRUE;
}

PRIM(string_to_barray)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  *result = gc_new_barray_from(BARRAY, x->barray.size, x->barray.elt);
  return TRUE;
}

PRIM(string_to_symbol)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  *result = gc_new_barray_from(SYMBOL, x->barray.size, x->barray.elt);
  return TRUE;
}

PRIM(string_to_keyword)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  *result = gc_new_barray_from(KEYWORD, x->barray.size, x->barray.elt);
  return TRUE;
}

PRIM(string_byte_length)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_string(argv->cons.car, &x)) return FALSE;
  *result = gc_new_xint(x->barray.size);
  return TRUE;
}

// symbol

PRIM(symbol_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, SYMBOL));
  return TRUE;
}

PRIM(symbol_add)
{
  object x;
  if (!ip_ensure_arguments(argc, 1, FALSE)) return FALSE;
  if (!ensure_symbol(argv->cons.car, result)) return FALSE;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!ensure_symbol(argv->cons.car, &x)) return FALSE;
    barray_add(STRING, x, result);
  }
  *result = gc_new_barray_from(SYMBOL, (*result)->barray.size
      , (*result)->barray.elt);
  return TRUE;
}

PRIM(symbol_to_string)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_symbol(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray_from(STRING, o->barray.size, o->barray.elt);
  return TRUE;
}

PRIM(symbol_to_keyword)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_symbol(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray_from(KEYWORD, o->barray.size, o->barray.elt);
  return TRUE;
}

// keyword

PRIM(keyword_p)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  *result = object_bool(typep(argv->cons.car, KEYWORD));
  return TRUE;
}

PRIM(keyword_to_symbol)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_keyword(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray_from(SYMBOL, o->barray.size, o->barray.elt);
  return TRUE;
}

PRIM(keyword_to_string)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_keyword(argv->cons.car, &o)) return FALSE;
  *result = gc_new_barray(STRING, o->barray.size + 1);
  (*result)->barray.elt[0] = 0x3a;
  memcpy((*result)->barray.elt + 1, o->barray.elt, o->barray.size);
  return TRUE;
}
