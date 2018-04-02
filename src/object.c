// paren object

#include <stdint.h>
#include <string.h>

#include "std.h"
#include "xsplay.h"
#include "lex.h"
#include "object.h"

object object_nil;
object object_true;
object object_false;
object object_error;
object object_pre_condition_error;
object object_post_condition_error;
object object_argument_error;

object object_car(object o)
{
  xassert(object_listp(o));
  if (object_nilp(o)) return object_nil;
  return o->cons.car;
}

object object_cdr(object o)
{
  xassert(object_listp(o));
  if (object_nilp(o)) return object_nil;
  return o->cons.cdr;
}

object object_nth(object o, int n)
{
  xassert(object_listp(o) && n >= 0);
  while (n-- != 0) o = object_cdr(o);
  return object_car(o);
}

int object_length(object o)
{
  xassert(object_listp(o));
  int c;
  for (c = 0; !object_nilp(o); o = object_cdr(o)) c++;
  return c;
}

int object_typep(object o, enum object_type type)
{
  xassert(o != NULL);
  return o->header.type == type;
}

int object_nilp(object o)
{
  xassert(o != NULL);
  return o == object_nil;
}

int object_consp(object o)
{
  xassert(o != NULL);
  return o->header.type == cons;
}

int object_listp(object o)
{
  xassert(o != NULL);
  return object_consp(o) || object_nilp(o);
}

int object_bool(object o) {
  xassert(o != NULL);
  switch (o->header.type) {
    case lambda:
    case cons:
    case keyword:
      return TRUE;
    case fbarray:
      return o->fbarray.size != 0;
    case farray:
      return o->farray.size != 0;
    case xint:
      return o->xint.val != 0;
    case xfloat:
      return o->xfloat.val != 0.0;
    case symbol:
      return object_nilp(o) || o == object_false;
  }
}

static void dump_s_expr(object o);

static void dump_cons(object o)
{
  xassert(object_consp(o));
  printf("(");
  dump_s_expr(object_car(o));
  while (!object_nilp(object_cdr(o))) {
    printf(" ");
    o = object_cdr(o);
    if (object_listp(o)) dump_s_expr(object_car(o));
    else {
      printf(". ");
      dump_s_expr(o);
      break;
    }
  }
  printf(")");
}

static void dump_s_expr(object o)
{
  int i;
  switch (o->header.type) {
    case lambda:
      printf("(lambda ");
      dump_s_expr(o->lambda.params);
      printf(" ");
      dump_s_expr(o->lambda.body);
      printf(")");
      break;
    case cons:
      dump_cons(o);
      break;
    case fbarray:
      printf("%s", o->fbarray.elt);
      break;
    case farray:
      printf("[");
      for (i = 0; i < o->farray.size; i++) dump_s_expr(o->farray.elt[i]);
      printf("]");
      break;
    case xint:
      printf("%lld", o->xint.val);
      break;
    case xfloat:
      printf("%f", o->xfloat.val);
      break;
    case symbol:
    case keyword:
      printf("%s", o->symbol.name);
      break;
  }
}

void object_dump(object o)
{
  xassert(o != NULL);
  dump_s_expr(o);
  printf("\n");
}
