// OS class

#include "std.h"

#include <time.h>
#include <string.h>
#include <errno.h>

#include "xbarray.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "pf.h"

PRIM(os_fp)
{
  FILE *fp;
  object o;
  FETCH_ARG_AS(o,XINT);
  switch (o->xint.val) {
    case 0: fp = stdin; break;
    case 1: fp = stdout; break;
    case 2: fp = stderr; break;
    default: return FALSE;
  }
  *result = gc_new_xint((intptr_t)fp);
  return TRUE;
}
