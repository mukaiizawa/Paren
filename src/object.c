// paren object

#include "std.h"
#include "xarray.h"
#include "xsplay.h"
#include "lex.h"
#include "object.h"

object object_toplevel;
object object_boot;
object object_nil;
object object_true;
object object_false;
object object_opt;
object object_key;
object object_rest;

object object_if;
object object_quote;
object object_assign;
object object_lambda;

static void dump_s_expr(object o);

static void dump_cons(object o)
{
  dump_s_expr(o->cons.car);
  while ((o = o->cons.cdr) != object_nil) {
    printf(" ");
    if (typep(o, Cons)) dump_s_expr(o->cons.car);
    else {
      printf(". ");
      dump_s_expr(o);
      break;
    }
  }
}

extern char *prim_name_table[];

int symcmp(object o, object p)
{
  intptr_t i;
  xassert(typep(o, Symbol) && typep(p, Symbol));
  if ((i = (intptr_t)o - (intptr_t)p) == 0) return 0;
  if (i > 0) return 1;
  return -1;
}

static void dump_s_expr(object o)
{
  int i;
  switch (type(o)) {
    case Lambda:
      printf("(lambda ");
      i = o->lambda.prim_cd;
      if (i >= 0)
        printf("<primitive:%s>", prim_name_table[i]);
      else {
        if (o->lambda.params == object_nil) printf("()");
        else dump_s_expr(o->lambda.params);
        if (o->lambda.body != object_nil) {
          printf(" ");
          dump_cons(o->lambda.body);
        }
      }
      printf(")");
      break;
    case Cons:
      printf("(");
      dump_cons(o);
      printf(")");
      break;
    case Fbarray:
      printf("%s", o->fbarray.elt);
      break;
    case Farray:
      printf("[");
      for (i = 0; i < o->farray.size; i++) dump_s_expr(o->farray.elt[i]);
      printf("]");
      break;
    case Xint:
      printf("%"PRId64"", o->xint.val);
      break;
    case Xfloat:
      printf("%f", o->xfloat.val);
      break;
    case Symbol:
    case Keyword:
      printf("%s", o->symbol.name);
      break;
    default: xerror("unknown type '%d'", type(o));
  }
}

void object_dump(object o)
{
  xassert(o != NULL);
  dump_s_expr(o);
  printf("\n");
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
  while (n != 0) {
    if (o == object_nil) return object_nil;
    o = o->cons.cdr;
  }
  return o->cons.car;
}

object object_bool(int b)
{
  if (b) return object_true;
  return object_false;
}
