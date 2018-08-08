// basic builtin

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

PRIM(type)
{
  if (argc != 1) return FALSE;
  switch (type(argv->cons.car)) {
    case Macro:
      *result = gc_new_symbol(":macro");
      break;
    case Lambda:
      *result = gc_new_symbol(":lambda");
      break;
    case Cons:
      *result = gc_new_symbol(":cons");
      break;
    case Xint:
    case Xfloat:
      *result = gc_new_symbol(":number");
      break;
    case Symbol:
      *result = gc_new_symbol(":symbol");
      break;
    case Keyword:
      *result = gc_new_symbol(":keyword");
      break;
    default:
      return FALSE;
  }
  return TRUE;
}

PRIM(samep)
{
  int b;
  object o;
  if (argc < 2) return FALSE;
  o = argv->cons.car;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!(b = o == argv->cons.car)) break;
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
  int t;
  if (o == p) return TRUE;
  if ((t = type(o)) != type(p)) return FALSE;
  switch (t) {
    case Cons: return equal_cons_p(o, p);
    case Xint: return o->xint.val == p->xint.val;
    case Xfloat: return o->xfloat.val == p->xfloat.val;
    default: return FALSE;
  }
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

PRIM(atomp)
{
  if (argc != 1) return FALSE;
  *result = object_bool(!typep(argv->cons.car, Cons));
  return TRUE;
}

// should be removed.
PRIM(print)
{
  char buf[MAX_STR_LEN];
  if (argc != 1) return FALSE;
  *result = argv->cons.car;
  printf("%s\n", object_describe(*result, buf));
  return TRUE;
}

PRIM(quit)
{
  printf("paren exit");
  exit(1);
}
