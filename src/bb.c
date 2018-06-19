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
      *result = gc_new_symbol(":Macro");
      break;
    case Lambda:
      *result = gc_new_symbol(":Lambda");
      break;
    case Cons:
      *result = gc_new_symbol(":Cons");
      break;
    case Xint:
    case Xfloat:
      *result = gc_new_symbol(":Number");
      break;
    case Symbol:
      *result = gc_new_symbol(":Symbol");
      break;
    case Keyword:
      *result = gc_new_symbol(":Keyword");
      break;
    default:
      return FALSE;
  }
  return TRUE;
}

PRIM(samep)
{
  int b;
  object o, p;
  if (argc < 2) return FALSE;
  while (argv->cons.cdr != object_nil) {
    o = argv->cons.car;
    p = (argv = argv->cons.cdr)->cons.car;
    if (!(b = o == p)) break;
  }
  *result = object_bool(b);
  return TRUE;
}

PRIM(equalp)
{
  int b;
  object o, p;
  if (argc < 2) return FALSE;
  while (argv->cons.cdr != object_nil) {
    o = argv->cons.car;
    p = (argv = argv->cons.cdr)->cons.car;
    if ((b = type(o) == type(p))) {
      switch (type(o)) {
        case Macro:
        case Lambda:
        case Cons:
        case Symbol:
        case Keyword:
          b = o == p;
          break;
        case Xint:
          b = o->xint.val == p->xint.val;
          break;
        case Xfloat:
          b = o->xfloat.val == p->xfloat.val;
          break;
        default: return FALSE;
      }
    }
    if (!b) break;
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
