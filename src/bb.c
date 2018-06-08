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
  object o, p;
  if (argc != 2) return FALSE;
  o = object_nth(argv, 0);
  p = object_nth(argv, 1);
  *result = object_bool(o == p);
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
