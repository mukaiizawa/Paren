// paren object.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

object object_toplevel;
object object_nil;
object object_true;
object object_key;
object object_opt;
object object_rest;
object object_quote;
object object_stack_trace;

object object_Class;
object object_Exception;
object object_class;
object object_symbol;
object object_super;
object object_features;
object object_fields;
object object_message;

int object_byte_size(object o)
{
  switch (object_type(o)) {
    case XINT:
      return sizeof(struct xint);
    case XFLOAT:
      return sizeof(struct xfloat);
    case CONS:
      return sizeof(struct cons);
    case SYMBOL:
    case KEYWORD:
    case STRING:
    case BYTES:
      return sizeof(struct mem) + o->mem.size - 1;
    case ARRAY:
      return sizeof(struct array) + sizeof(object) * (o->array.size - 1);
    case MACRO:
    case FUNC:
      return sizeof(struct proc);
    case ENV:
    case DICT:
      return sizeof(struct map);
    default:
      xassert(FALSE);
      return -1;
  }
}

static void xbarray_add_mem(struct xbarray *x, object o)
{
  memcpy(xbarray_reserve(x, o->mem.size), o->mem.elt, o->mem.size);
}

static void describe_s_expr(object o, struct xbarray *x);
static void describe_cons(object o, struct xbarray *x)
{
  describe_s_expr(o->cons.car, x);
  while ((o = o->cons.cdr) != object_nil) {
    xbarray_add(x, ' ');
    describe_s_expr(o->cons.car, x);
    if (x->size > MAX_STR_LEN) return;
  }
}

static void describe_mem(object o, struct xbarray *x)
{
  int i;
  xbarray_adds(x, "#[");
  for (i = 0; i < o->mem.size; i++) {
    xbarray_addf(x, "0x%x ", LC(o->mem.elt + i));
    if (x->size > MAX_STR_LEN) return;
  }
  xbarray_add(x, ']');
}

static void describe_array(object o, struct xbarray *x)
{
  int i;
  xbarray_adds(x, "#[ ");
  for (i = 0; i < o->array.size; i++) {
    describe_s_expr(o->array.elt[i], x);
    xbarray_add(x, ' ');
    if (x->size > MAX_STR_LEN) return;
  }
  xbarray_add(x, ']');
}

static void describe_map(object o, struct xbarray *x)
{
  int i;
  object p;
  xbarray_adds(x, "#{ ");
  for (i = 0; i < o->map.half_size; i++) {
    if ((p = o->map.table[i]) == NULL) continue;
    describe_s_expr(o->map.table[i], x);
    xbarray_add(x, ' ');
    describe_s_expr(o->map.table[i + o->map.half_size], x);
    xbarray_add(x, ' ');
    if (x->size > MAX_STR_LEN) return;
  }
  xbarray_add(x, '}');
}

static void describe_s_expr(object o, struct xbarray *x)
{
  if (x->size > MAX_STR_LEN) return;
  switch (object_type(o)) {
    case SINT:
      xbarray_addf(x, "%d", sint_val(o));
      break;
    case XINT:
      xbarray_addf(x, "%" PRId64, o->xint.val);
      break;
    case XFLOAT:
      xbarray_addf(x, "%g", o->xfloat.val);
      break;
    case CONS:
      xbarray_add(x, '(');
      describe_cons(o, x);
      xbarray_add(x, ')');
      break;
    case STRING:
      xbarray_add(x, '"');
      xbarray_add_mem(x, o);
      xbarray_add(x, '"');
      break;
    case KEYWORD:
      xbarray_add(x, ':');
      xbarray_add_mem(x, o);
      break;
    case BUILTINFUNC:
    case SPECIAL:
      o = o->builtin.name;
    case SYMBOL:
      xbarray_add_mem(x, o);
      break;
    case BYTES:
      describe_mem(o, x);
      break;
    case ARRAY:
      describe_array(o, x);
      break;
    case DICT:
      describe_map(o, x);
      break;
    case MACRO:
    case FUNC:
      if (object_type_p(o, MACRO)) xbarray_adds(x, "(macro ");
      else xbarray_adds(x, "(f ");
      if (o->proc.params == object_nil) xbarray_adds(x, "()");
      else describe_s_expr(o->proc.params, x);
      if (o->proc.body != object_nil) {
        xbarray_add(x, ' ');
        describe_cons(o->proc.body, x);
      }
      xbarray_add(x, ')');
      break;
    case ENV:
      xbarray_addf(x, "#(:environment 0x%p :top 0x%p)", o, o->map.top);
      break;
    default:
      xassert(FALSE);
  }
}

char *object_describe(object o, char *buf)
{
  struct xbarray x;
  xassert(o != NULL);
  xbarray_init(&x);
  describe_s_expr(o, &x);
  xbarray_add(&x,'\0');
  if (x.size <= MAX_STR_LEN) memcpy(buf, x.elt, x.size);
  else {
    memcpy(buf, x.elt, MAX_STR_LEN - 4);
    strcpy(buf + MAX_STR_LEN - 4, "...");
  }
  xbarray_free(&x);
  return buf;
}

object object_bool(int b)
{
  if (b) return object_true;
  return object_nil;
}

static int dbl_eq(double x, object p)
{
  int64_t i;
  double d;
  if (bi_cint64(p, &i)) return fabs(x - (double)i) < DBL_EPSILON;
  if (bi_cdouble(p, &d)) return fabs(x - d) < DBL_EPSILON;
  return FALSE;
}

static int int64eq_p(int64_t x, object p)
{
  int64_t y;
  if (bi_cint64(p, &y)) return x == y;
  return dbl_eq((double)x, p);
}

static int numeq_p(object o, object p)
{
  int64_t i;
  double d;
  if (bi_cint64(o, &i)) return int64eq_p(i, p);
  if (bi_cdouble(o, &d)) return dbl_eq(d, p);
  return FALSE;
}

static int dict_eq_p(object o, object p)
{
  object v, keys;
  if (o->map.entry_count != p->map.entry_count) return FALSE;
  keys = map_keys(o);
  while (keys != object_nil) {
    if ((v = map_get(p, keys->cons.car)) == NULL) return FALSE;
    if (!object_eq_p(map_get(o, keys->cons.car), v)) return FALSE;
    keys = keys->cons.cdr;
  }
  return TRUE;
}

int object_eq_p(object o, object p)
{
  int i;
  if (o == p) return TRUE;
  switch (object_type(o)) {
    case SINT:
    case XINT:
    case XFLOAT:
      return numeq_p(o, p);
    case BYTES:
    case STRING:
      if (object_type(o) != object_type(p)) return FALSE;
      if (o->mem.size != p->mem.size) return FALSE;
      return memcmp(o->mem.elt, p->mem.elt, o->mem.size) == 0;
    case CONS:
      if (!object_type_p(p, CONS)) return FALSE;
      while (o != object_nil) {
        if (p == object_nil) return FALSE;
        if (!object_eq_p(o->cons.car, p->cons.car)) return FALSE;
        o = o->cons.cdr;
        p = p->cons.cdr;
      }
      return TRUE;
    case ARRAY:
      if (!object_type_p(p, ARRAY)) return FALSE;
      if (o->array.size != p->array.size) return FALSE;
      for (i = 0; i < o->array.size; i++)
        if (!object_eq_p(o->cons.car, p->cons.car)) return FALSE;
      return TRUE;
    case DICT:
      if (!object_type_p(p, DICT)) return FALSE;
      return dict_eq_p(o, p);
    default:
      return FALSE;
  }
}

int list_len(object o)
{
  int i;
  xassert(list_p(o));
  for (i = 0; object_type_p(o, CONS); i++) o = o->cons.cdr;
  return i;
}

object list_reverse(object o)
{
  object p, acc;
  xassert(list_p(o));
  acc = object_nil;
  while (o != object_nil) {
    p = o->cons.cdr;
    o->cons.cdr = acc;
    acc = o;
    o = p;
  }
  return acc;
}

int ch_len(unsigned char ch, int *len)
{
  if (ch < 0x80) (*len) += 1;
  else if (ch < 0xe0) (*len) += 2;
  else if (ch < 0xf0) (*len) += 3;
  else if (ch < 0xf8) (*len) += 4;
  else return ip_throw(ArgumentError, invalid_utf8_byte_sequense);
  return TRUE;
}

int str_len(object o, int *len)
{
  int i;
  i = *len = 0;
  while (i < o->mem.size) {
    if (!ch_len(LC(o->mem.elt + i), &i)) return FALSE;
    (*len)++;
  }
  return TRUE;
}

int str_slice(object o, int start, int stop, object *result)
{
  int i, s, t;
  for (i = s = 0; i < start; i++)
    if (!ch_len(LC(o->mem.elt + s), &s)) return FALSE;
  if (stop == -1) t = o->mem.size;
  else {
    for (i = start, t = s; i < stop; i++) {
      if (!ch_len(LC(o->mem.elt + t), &t)) return FALSE;
    }
  }
  *result = gc_new_mem_from(STRING, o->mem.elt + s, t - s);
  return TRUE;
}

object map_get(object o, object s)
{
  int i;
  object p;
  i = object_hash(s) % o->map.half_size;
  while ((p = o->map.table[i]) != NULL) {
    if (p == s) return o->map.table[i + o->map.half_size];
    if (++i == o->map.half_size) i = 0;
  }
  return NULL;
}

object map_get_propagation(object o, object s)
{
  object p;
  while (o != object_nil) {
    if ((p = map_get(o, s)) != NULL) return p;
    o = o->map.top;
  }
  return NULL;
}

void map_put(object o, object s, object v)
{
  int i;
  object p;
  i = object_hash(s) % o->map.half_size;
  while ((p = o->map.table[i]) != NULL) {
    if (p == s) {
      o->map.table[i + o->map.half_size] = v;
      return;
    }
    if (++i == o->map.half_size) i = 0;
  }
  o->map.table[i] = s;
  o->map.table[i + o->map.half_size] = v;
  o->map.entry_count++;
  if (o->map.entry_count * 2 > o->map.half_size) gc_extend_table(o);
}

void map_put_propagation(object o, object s, object v)
{
  while (o != object_toplevel) {
    if (map_get(o, s) != NULL) {
      map_put(o, s, v);
      return;
    }
    o = o->map.top;
  }
  map_put(object_toplevel, s, v);
}

void map_foreach(object o, void (*f)(void *s, void *v))
{
  int i;
  object *table;
  table = o->map.table;
  for (i = 0; i < o->map.half_size; i++)
    if (table[i] != NULL) (*f)(table[i], table[i + o->map.half_size]);
}

object map_keys(object o)
{
  int i;
  object keys, *table;
  xassert(object_type_p(o, DICT));
  table = o->map.table;
  keys = object_nil;
  for (i = 0; i < o->map.half_size; i++)
    if (table[i] != NULL) keys = gc_new_cons(table[i], keys);
  return keys;
}
