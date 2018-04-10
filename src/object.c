// paren object

#include <stdint.h>
#include <string.h>

#include "std.h"
#include "xarray.h"
#include "xsplay.h"
#include "lex.h"
#include "object.h"

struct xarray object_table;

object object_nil;
object object_true;
object object_false;

object object_alloc(void)
{
  return xmalloc(sizeof(union s_expr));
}

static void dump_s_expr(object o);

static void dump_cons(object o)
{
  printf("(");
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
  printf(")");
}

char *object_type_name[] = {
  "lambda",
  "cons",
  "fbarray",
  "farray",
  "xint",
  "xfloat",
  "symbol",
  "symbol",
  "keyword"
};

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
        dump_s_expr(o->lambda.params);
        printf(" ");
        dump_s_expr(o->lambda.body);
      }
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

void object_init(void)
{
  xarray_init(&object_table);
}
