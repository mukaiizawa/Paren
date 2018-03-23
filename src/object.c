// paren object

#include <stdint.h>

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
  object o;
  o = object_alloc(sizeof(struct symbol));
  o->header.type = symbol;
  o->symbol.name = name;
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

void object_dump(object o)
{
  xassert(o != NULL);
  switch (o->header.type) {
    case cons: printf("a cons\n"); break;
    case xint: printf("%lld\n", o->xint.val); break;
    case xfloat: printf("%f\n", o->xfloat.val); break;
    case keyword: printf("%s\n", o->keyword.name); break;
    case symbol: printf("%s\n", o->symbol.name); break;
  }
}
