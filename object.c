// paren object

#include "std.h"
#include "xbarray.h"
#include "object.h"

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
    case ENV:
      return sizeof(struct env);
    case MACRO:
    case FUNC:
      return sizeof(struct proc);
    case CONS:
      return sizeof(struct cons);
    case XINT:
      return sizeof(struct xint);
    case XFLOAT:
      return sizeof(struct xfloat);
    case SYMBOL:
    case KEYWORD:
    case STRING:
    case BYTES:
      return sizeof(struct mem) + o->mem.size - 1;
    case ARRAY:
      return sizeof(struct array) + (o->array.size - 1) * sizeof(object);
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
  xbarray_adds(x, "#b[");
  for (i = 0; i < o->mem.size; i++) {
    if (i != 0) xbarray_add(x, ' ');
    xbarray_addf(x, "0x%x", o->mem.elt[i]);
    if (x->size > MAX_STR_LEN) return;
  }
  xbarray_add(x, ']');
}

static void describe_array(object o, struct xbarray *x)
{
  int i;
  xbarray_adds(x, "#a[");
  for (i = 0; i < o->array.size; i++) {
    if (i != 0) xbarray_add(x, ' ');
    describe_s_expr(o->array.elt[i], x);
    if (x->size > MAX_STR_LEN) return;
  }
  xbarray_add(x, ']');
}

static void describe_s_expr(object o, struct xbarray *x)
{
  object p;
  if (x->size > MAX_STR_LEN) return;
  switch (object_type(o)) {
    case ENV:
      xbarray_addf(x, "#(:environment 0x%p :top 0x%p)", o, o->env.top);
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
    case CONS:
      p = o->cons.car;
      if (p == object_quote && object_type_p(o->cons.cdr, CONS)
          && o->cons.cdr->cons.cdr == object_nil)
      {
        xbarray_add(x, '\'');
        describe_s_expr(o->cons.cdr->cons.car, x);
      } else {
        xbarray_add(x, '(');
        describe_cons(o, x);
        xbarray_add(x, ')');
      }
      break;
    case SINT:
      xbarray_addf(x, "%d", sint_val(o));
      break;
    case XINT:
      xbarray_addf(x, "%" PRId64, o->xint.val);
      break;
    case XFLOAT:
      xbarray_addf(x, "%g", o->xfloat.val);
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
    default: xassert(FALSE);
  }
}

char *object_describe(object o, char *buf)
{
  struct xbarray x;
  xassert(o != NULL);
  xbarray_init(&x);
  describe_s_expr(o, &x);
  xbarray_add(&x,'\0');
  if(x.size <= MAX_STR_LEN) memcpy(buf, x.elt, x.size);
  else {
    memcpy(buf, x.elt, MAX_STR_LEN - 4);
    strcpy(buf + MAX_STR_LEN - 4, "...");
  }
  xbarray_free(&x);
  return buf;
}
