// paren object

#include "std.h"
#include "xarray.h"
#include "xsplay.h"
#include "lex.h"
#include "object.h"

object toplevel;
object object_nil;
object object_true;
object object_false;
object object_opt;
object object_key;
object object_rest;

static void dump_s_expr(object o);

static void dump_cons(object o)
{
  dump_s_expr(o->cons.car);
  while ((o = o->cons.cdr) != object_nil) {
    printf(" ");
    if (o->header.type == cons) dump_s_expr(o->cons.car);
    else {
      printf(". ");
      dump_s_expr(o);
      break;
    }
  }
}

extern char *prim_name_table[];

static void dump_s_expr(object o)
{
  int i;
  switch (o->header.type) {
    case lambda:
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
    case cons:
      printf("(");
      dump_cons(o);
      printf(")");
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
      printf("%"PRId64"", o->xint.val);
      break;
    case xfloat:
      printf("%f", o->xfloat.val);
      break;
    case symbol:
    case keyword:
      printf("%s", o->symbol.name);
      break;
    default: xerror("unknown type '%d'", o->header.type);
  }
}

void object_dump(object o)
{
  xassert(o != NULL);
  dump_s_expr(o);
  printf("\n");
}

object object_bool(int b)
{
  if (b) return object_true;
  return object_false;
}
