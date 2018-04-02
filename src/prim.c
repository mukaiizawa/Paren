// special operators.

#include <stdint.h>

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "prim.h"

PRIM(quote)
{
  if (object_length(*args) != 1) return FALSE;
  *result = object_nth(*args, 0);
  return TRUE;
}
