// special form

#include "std.h"
#include "object.h"
#include "prim.h"

PRIM(lambda)
{
  object params, param;
  if (argc == 0 || !object_listp(params = arg(0))) return FALSE;
  while (!object_nilp(params)) {    // TODO
    if (!object_typep(param = object_car(params), symbol)
        && object_typep(param, keyword))
      return FALSE;
    params = object_cdr(params);
    if (!object_listp(params)) return FALSE;
  }
  *result = new_lambda(arg(0), object_cdr(args), -1);
  return TRUE;
}

PRIM(assign)
{
  int i;
  if (argc % 2 != 0) return FALSE;
  for (i = 0; i <= argc / 2; i+= 2) {
    if (!object_typep(arg(i), symbol)) return FALSE;
    *result = eval(arg(i + 1));
    bind(arg(i), *result);
  }
  return TRUE;
}

PRIM(quote)
{
  if (argc != 1) return FALSE;
  *result = arg(0);
  return TRUE;
}

PRIM(if)
{
  if (argc != 2 && argc != 3) return FALSE;
  if (object_bool(arg(0))) *result = eval(arg(1));
  else if (argc > 2) *result = eval(arg(2));
  else *result = object_false;
  return TRUE;
}
