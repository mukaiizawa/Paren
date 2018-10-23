// paren object

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "xsplay.h"
#include "object.h"

object object_toplevel;
object object_boot;
object object_nil;
object object_true;
object object_opt;
object object_key;
object object_rest;
object object_quote;
object object_bq;
object object_uq;
object object_splice;
object object_not;
object object_sint[SINT_MAX];

int symcmp(object o, object p)
{
  intptr_t i;
  xassert((typep(o, Symbol) && typep(p, Symbol))
      || (typep(o, Keyword) && typep(p, Keyword)));
  if ((i = (intptr_t)o - (intptr_t)p) == 0) return 0;
  if (i > 0) return 1;
  return -1;
}

int object_length(object o)
{
  int i;
  xassert(listp(o));
  for (i = 0; typep(o, Cons); i++) o = o->cons.cdr;
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

static void describe_s_expr(object o, struct xbarray *x)
{
  object p;
  if(x->size > MAX_STR_LEN) return;
  switch (type(o)) {
    case Env:
      xbarray_addf(x, "<environment: %p>", o);
      break;
    case Macro:
    case Lambda:
      if (typep(o, Macro)) xbarray_adds(x, "(macro ");
      else xbarray_adds(x, "(lambda ");
      if (o->lambda.params == object_nil) xbarray_adds(x, "()");
      else describe_s_expr(o->lambda.params, x);
      if (o->lambda.body != object_nil) {
        xbarray_add(x, ' ');
        describe_cons(o->lambda.body, x);
      }
      xbarray_add(x, ')');
      break;
    case Cons:
      p = o->cons.car;
      if ((p == object_quote || p == object_bq || p == object_uq
            || p == object_splice || p == object_not)
          && typep(o->cons.cdr, Cons) && o->cons.cdr->cons.cdr == object_nil)
      {
        if (p == object_quote) xbarray_add(x, '\'');
        if (p == object_bq) xbarray_add(x, '`');
        if (p == object_uq) xbarray_add(x, ',');
        if (p == object_splice) xbarray_adds(x, ",@");
        if (p == object_not) xbarray_add(x, '!');
        describe_s_expr(o->cons.cdr, x);
      } else {
        xbarray_add(x, '(');
        describe_cons(o, x);
        xbarray_add(x, ')');
      }
      break;
    case Xint:
      xbarray_addf(x, "%d", o->xint.val);
      break;
    case Xfloat:
      xbarray_addf(x, "%f", o->xfloat.val);
      break;
    case Symbol:
    case Keyword:
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
