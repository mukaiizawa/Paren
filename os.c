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
  int64_t fd;
  FILE *fp;
  if (argc != 1 || !bi_int64(argv->cons.car, &fd)) return FALSE;
  switch (fd) {
    case 0: fp = stdin; break;
    case 1: fp = stdout; break;
    case 2: fp = stderr; break;
    default: return FALSE;
  }
  *result = gc_new_xint((intptr_t)fp);
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
  int64_t mode;
  FILE *fp;
  object ofn;
  if (!typep(ofn = argv->cons.car, BARRAY)) return FALSE;
  fn = ofn->barray.elt;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &mode)) return FALSE;
  if (0 < mode || mode >= sizeof(mode_table) / sizeof(char *)) return FALSE;
  if ((fp = fopen(fn, mode_table[mode])) == NULL) return FALSE;
  *result = gc_new_xint((intptr_t)fp);
  return TRUE;
}

PRIM(os_fgetc)
{
  int ch;
  FILE *fp;
  if (!bi_intptr(argv->cons.car,(intptr_t *)&fp)) return FALSE;
  ch = fgetc(fp);
  if (ch == EOF && ferror(fp)) {
    clearerr(fp);
    return FALSE;
  }
  *result = gc_new_xint(ch);
  return TRUE;
}
