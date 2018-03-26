// paren object

#include <stdint.h>
#include <string.h>

#include "std.h"
#include "lex.h"
#include "object.h"

object object_nil;
object object_true;
object object_false;

object object_alloc(int size)
{
  return xmalloc(size);
}

object object_new_cons(object car, object cdr)
{
  object o;
  o = object_alloc(sizeof(struct cons));
  o->header.type = cons;
  o->cons.car = car;
  o->cons.cdr = cdr;
  return o;
}

object object_new_symbol(char *name)
{
  int size;
  char* str;
  object o;
  o = object_alloc(sizeof(struct symbol));
  str = xmalloc(size = sizeof(char) * (strlen(name) + 1));
  memcpy(str, name, size);
  o->header.type = symbol;
  o->symbol.name = str;
  o->symbol.val = object_nil;
  return o;
}

object object_new_keyword(char *name)
{
  object o;
  o = object_alloc(sizeof(struct keyword));
  o->header.type = keyword;
  o->keyword.name = name;
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
  switch (o->header.type) {
    case cons: dump_cons(o); break;
    case xint: printf("%lld", o->xint.val); break;
    case xfloat: printf("%f", o->xfloat.val); break;
    case keyword: printf("%s", o->keyword.name); break;
    case symbol: printf("%s", o->symbol.name); break;
  }
}

void object_dump(object o)
{
  xassert(o != NULL);
  dump_s_expr(o);
  printf("\n");
}
