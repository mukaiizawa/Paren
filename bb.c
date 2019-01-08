// basic builtin

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

PRIM(gensym)
{
  static long index = 0;
  char buf[MAX_STR_LEN];
  if (argc != 0) return FALSE;
  sprintf(buf, "$G%ld", index++);
  *result = gc_new_symbol(stralloc(buf));
  return TRUE;
}

PRIM(samep)
{
  int b;
  object o;
  if (argc < 2) return FALSE;
  o = argv->cons.car;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!(b = (o == argv->cons.car))) break;
  }
  *result = object_bool(b);
  return TRUE;
}

static int equal_s_expr(object o, object p);
static int equal_cons_p(object o, object p)
{
  return equal_s_expr(o->cons.car, p->cons.car)
    && equal_s_expr(o->cons.cdr, p->cons.cdr);
}

static int equal_s_expr(object o, object p)
{
  double x, y;
  if (o == p) return TRUE;
  if (bi_double(o, &x) && bi_double(p, &y) && x == y) return TRUE;
  if (typep(o, CONS) && typep(p, CONS)) return equal_cons_p(o, p);
  return FALSE;
}

// compare built-in object.
PRIM(equalp)
{
  int b;
  object o, p;
  if (argc < 2) return FALSE;
  o = argv->cons.car;
  while ((argv = argv->cons.cdr) != object_nil) {
    p = argv->cons.car;
    if (!(b = equal_s_expr(o, p))) break;
  }
  *result = object_bool(b);
  return TRUE;
}

PRIM(type)
{
  if (argc != 1) return FALSE;
  switch (type(argv->cons.car)) {
    case MACRO:
      *result = gc_new_symbol(":macro");
      break;
    case LAMBDA:
      *result = gc_new_symbol(":lambda");
      break;
    case CONS:
      *result = gc_new_symbol(":cons");
      break;
    case XINT:
    case XFLOAT:
      *result = gc_new_symbol(":number");
      break;
    case SYMBOL:
      *result = gc_new_symbol(":symbol");
      break;
    case KEYWORD:
      *result = gc_new_symbol(":keyword");
      break;
    default:
      return FALSE;
  }
  return TRUE;
}

// TODO should be removed
PRIM(print)
{
  char buf[MAX_STR_LEN];
  if (argc != 1) return FALSE;
  *result = argv->cons.car;
  printf("%s\n", object_describe(*result, buf));
  return TRUE;
}

// TODO should be removed
PRIM(quit)
{
  printf("paren exit");
  exit(1);
}
