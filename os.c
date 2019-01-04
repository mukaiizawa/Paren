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
  *result = bi_xint((intptr_t)fp);
  return TRUE;
}

static char *mode_table[] = {
  "rb",
  "wb",
  "ab",
  "rb+"
};

PRIM(os_fopen)
{
  char *fn;
  int mode;
  FILE *fp;
  object ofn, omode;
  FETCH_ARG_AS(ofn, BARRAY);
  FETCH_ARG_AS(omode, XINT);
  fn = ofn->barray.elt;
  mode = omode->xint.val;
  if (0 < mode || mode >= sizeof(mode_table) / sizeof(char *)) return FALSE;
  if ((fp = fopen(fn, mode_table[mode])) == NULL) return FALSE;
  *result = bi_xint((intptr_t)fp);
  return TRUE;
}

// PRIM(os_fgetc)
// {
//   int ch;
//   FILE *fp;
//   FETCH_ARG_AS(omode, XINT);
//   if (!p_intptr_val(args[0],(intptr_t *)&fp)) return FALSE;
//   ch=fgetc(fp);
//   if(ch==EOF&&ferror(fp)) {
//     clearerr(fp);
//     return FALSE;
//   }
//   *result=sint(ch);
//   return TRUE;	
// }

