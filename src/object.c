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
object object_false;
object object_opt;
object object_key;
object object_rest;
object object_quote;

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

object object_nth(object o, int n)
{
  xassert(listp(o));
  while (n-- != 0) {
    if (o == object_nil) return object_nil;
    o = o->cons.cdr;
  }
  return o->cons.car;
}

static void describe_s_expr(object o, struct xbarray *x);
static void describe_cons(object o, struct xbarray *x)
{
  describe_s_expr(o->cons.car, x);
  while ((o = o->cons.cdr) != object_nil) {
    xbarray_add(x, ' ');
    if (typep(o, Cons)) describe_s_expr(o->cons.car, x);
    else {
      xbarray_adds(x, ". ");
      describe_s_expr(o, x);
      break;
    }
    if(x->size > MAX_STR_LEN) return;
  }
}

static void describe_s_expr(object o, struct xbarray *x)
{
  int i;
  if(x->size > MAX_STR_LEN) return;
  switch (type(o)) {
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
      xbarray_add(x, '(');
      describe_cons(o, x);
      xbarray_add(x, ')');
      break;
    case Fbarray:
      xbarray_addf(x, "%s", o->fbarray.elt);
      break;
    case Farray:
      xbarray_add(x, '[');
      for (i = 0; i < o->farray.size; i++)
        describe_s_expr(o->farray.elt[i], x);
      xbarray_add(x, ']');
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
    default: xerror("unknown type '%d'", type(o));
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

object object_bool(int b)
{
  if (b) return object_true;
  return object_false;
}
