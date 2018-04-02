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

object CAR(object o)
{
  if (o == object_nil) return o;
  xassert(TYPEP(o, cons));
  return o->cons.car;
}

object CDR(object o)
{
  if (o == object_nil) return o;
  xassert(TYPEP(o, cons));
  return o->cons.cdr;
}

int TYPEP(object o, enum object_type type)
{
  return o->header.type == type;
}

int NILP(object o)
{
  return o == object_nil;
}

int CONP(object o)
{
  xassert(o != NULL);
  return o->header.type == cons;
}

int listp(object o)
{
  xassert(o != NULL);
  return CONP(o) || NILP(o);
}

static void dump_s_expr(object o);

static void dump_cons(object o)
{
  xassert(CONP(o));
  printf("(");
  dump_s_expr(CAR(o));
  while (!NILP(CDR(o))) {
    printf(" ");
    o = CDR(o);
    if (listp(o)) dump_s_expr(CAR(o));
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
