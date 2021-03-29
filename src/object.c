// paren object.

#include "std.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"

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
object object_Error;
object object_SystemExit;

object object_class;
object object_symbol;
object object_super;
object object_features;
object object_fields;
object object_message;

int object_list_len(object o)
{
  int i;
  xassert(object_list_p(o));
  for (i = 0; object_type_p(o, CONS); i++) o = o->cons.cdr;
  return i;
}

object object_bool(int b)
{
  if (b) return object_true;
  return object_nil;
}

object object_reverse(object o)
{
  object p, acc;
  xassert(object_list_p(o));
  acc = object_nil;
  while (o != object_nil) {
    p = o->cons.cdr;
    o->cons.cdr = acc;
    acc = o;
    o = p;
  }
  return acc;
}

int object_list_p(object o)
{
  if (o == object_nil) return TRUE;
  return object_type_p(o, CONS);
}

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


object object_find(object o, object s)
{
  int i;
  object p;
  i = hash(s) % o->map.half_size;
  while ((p = o->map.table[i]) != NULL) {
    if (p == s) return o->map.table[i + o->map.half_size];
    if (++i == o->map.half_size) i = 0;
  }
  return NULL;
}

object object_find_propagation(object o, object s)
{
  object p;
  while (o != object_nil) {
    if ((p = object_find(o, s)) != NULL) return p;
    o = o->map.top;
  }
  return NULL;
}

void object_bind(object o, object s, object v)
{
  int i;
  object p;
  i = hash(s) % o->map.half_size;
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

void object_bind_propagation(object o, object s, object v)
{
  while (o != object_toplevel) {
    if (object_find(o, s) != NULL) {
      object_bind(o, s, v);
      return;
    }
    o = o->map.top;
  }
  object_bind(object_toplevel, s, v);
}

void object_map_foreach(object o, void (*f)(void *s, void *v))
{
  int i;
  object *table;
  table = o->map.table;
  for (i = 0; i < o->map.half_size; i++)
    if (table[i] != NULL) (*f)(table[i], table[i + o->map.half_size]);
}

object object_map_keys(object o)
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
