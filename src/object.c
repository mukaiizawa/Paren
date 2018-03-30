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

object object_alloc()
{
  return xmalloc(sizeof(union s_expr));
}

object object_new_cons(object car, object cdr)
{
  object o;
  o = object_alloc();
  o->header.type = cons;
  o->cons.car = car;
  o->cons.cdr = cdr;
  return o;
}

object object_new_barray(int len)
{
  object o;
  o = xmalloc(sizeof(struct fbarray) + len - 1);
  o->header.type = fbarray;
  return o;
}

object object_new_fbarray(int len)
{
  object o;
  o = xmalloc(sizeof(struct farray) + (len - 1) * sizeof(object));
  o->header.type = farray;
  while (len-- > 0) o->farray.elt[len] = object_nil;
  return o;
}

object object_new_xint(int val)
{
  object o;
  o = object_alloc();
  o->header.type = xint;
  o->xint.val = val;
  return o;
}

object object_new_xfloat(double val)
{
  object o;
  o = object_alloc();
  o->header.type = xfloat;
  o->xfloat.val = val;
  return o;
}

int object_nilp(object o)
{
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

static void dump_s_expr(object o);

static void dump_cons(object o)
{
  xassert(object_consp(o));
  printf("(");
  dump_s_expr(o->cons.car);
  while (!object_nilp(o->cons.cdr)) {
    printf(" ");
    o = o->cons.cdr;
    if (object_listp(o)) dump_s_expr(o->cons.car);
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
      dump_cons(o->lambda.params);
      printf("\n\t");
      dump_cons(o->lambda.body);
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
