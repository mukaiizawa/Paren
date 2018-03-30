// special operators.

#include <stdint.h>

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "prim.h"

int argc(object args)
{
  int c;
  for (c = 0; args != object_nil; args = args->cons.cdr) c++;
  return c;
}

object nth(object args, int n)
{
  while (n != 0) args = args->cons.cdr;
  return args->cons.car;
}

PRIM(quote)
{
  if (argc(*args) != 1) return FALSE;
  *result = nth(*args, 0);
  return TRUE;
}
