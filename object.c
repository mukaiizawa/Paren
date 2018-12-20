// paren object

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "xsplay.h"
#include "object.h"

object object_boot;
object object_catch;
object object_finally;
object object_key;
object object_nil;
object object_not;
object object_opt;
object object_quote;
object object_rest;
object object_bytes[256];
object object_snbhe;
object object_st;
object object_toplevel;
object object_true;

int symcmp(object o, object p)
{
  intptr_t i;
  xassert((typep(o, SYMBOL) && typep(p, SYMBOL))
      || (typep(o, KEYWORD) && typep(p, KEYWORD)));
  if ((i = (intptr_t)o - (intptr_t)p) == 0) return 0;
  if (i > 0) return 1;
  return -1;
}

int object_length(object o)
{
  int i;
  xassert(listp(o));
  for (i = 0; typep(o, CONS); i++) o = o->cons.cdr;
  return i;
}

object object_bool(int b)
{
  if (b) return object_true;
  return object_nil;
}

object object_nth(object o, int n)
{
  xassert(listp(o));
  while (n-- != 0) {
    if (o == object_nil) return object_nil;
    o = o->cons.cdr;
  }
  return o->cons.car;
}

object object_reverse(object o)
{
  object p, acc;
  xassert(listp(o));
  acc = object_nil;
  while (o != object_nil) {
    p = o->cons.cdr;
    o->cons.cdr = acc;
    acc = o;
    o = p;
  }
  return acc;
}

static void describe_s_expr(object o, struct xbarray *x);
static void describe_cons(object o, struct xbarray *x)
{
  describe_s_expr(o->cons.car, x);
  while ((o = o->cons.cdr) != object_nil) {
    xbarray_add(x, ' ');
    describe_s_expr(o->cons.car, x);
    if(x->size > MAX_STR_LEN) return;
  }
}

static void describe_barray(object o, struct xbarray *x)
{
  int i;
  xbarray_adds(x, "#b[");
  for (i = 0; i < o->barray.size; i++) {
    if (i != 0) xbarray_add(x, ' ');
    xbarray_addf(x, "0x%d", (int)(o->barray.elt[i]));
  }
  xbarray_add(x, ']');
}

static void describe_s_expr(object o, struct xbarray *x)
{
  object p;
  if(x->size > MAX_STR_LEN) return;
  switch (type(o)) {
    case ENV:
      xbarray_addf(x, "<environment: %p>", o);
      break;
    case MACRO:
    case LAMBDA:
      if (typep(o, MACRO)) xbarray_adds(x, "(macro ");
      else xbarray_adds(x, "(lambda ");
      if (o->lambda.params == object_nil) xbarray_adds(x, "()");
      else describe_s_expr(o->lambda.params, x);
      if (o->lambda.body != object_nil) {
        xbarray_add(x, ' ');
        describe_cons(o->lambda.body, x);
      }
      xbarray_add(x, ')');
      break;
    case CONS:
      p = o->cons.car;
      if ((p == object_quote || p == object_not)
          && typep(o->cons.cdr, CONS) && o->cons.cdr->cons.cdr == object_nil)
      {
        if (p == object_quote) xbarray_add(x, '\'');
        if (p == object_not) xbarray_add(x, '!');
        describe_s_expr(o->cons.cdr->cons.car, x);
      } else {
        xbarray_add(x, '(');
        describe_cons(o, x);
        xbarray_add(x, ')');
      }
      break;
    case XINT:
      xbarray_addf(x, "%d", o->xint.val);
      break;
    case XFLOAT:
      xbarray_addf(x, "%f", o->xfloat.val);
      break;
    case BARRAY:
      describe_barray(o, x);
      break;
    case SYMBOL:
    case KEYWORD:
      xbarray_addf(x, "%s", o->symbol.name);
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
