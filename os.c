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
  if (argc != 2) return FALSE;
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
  if (argc != 1) return FALSE;
  if (!bi_intptr(argv->cons.car,(intptr_t *)&fp)) return FALSE;
  ch = fgetc(fp);
  if (ch == EOF && ferror(fp)) {
    clearerr(fp);
    return FALSE;
  }
  *result = gc_new_xint(ch);
  return TRUE;
}

PRIM(os_fputc)
{
  int64_t byte;
  FILE *fp;
  if (argc != 2) return FALSE;
  if (!bi_int64(argv->cons.car, &byte) || !byte_range_p(byte)) return FALSE;
  if (!bi_intptr(argv->cons.cdr->cons.car,(intptr_t *)&fp)) return FALSE;
  if (fputc((int)byte, fp) == EOF) return FALSE;
  return TRUE;
}

PRIM(os_fgets)
{
  char *s;
  FILE *fp;
  struct xbarray x;
  if (argc != 1) return FALSE;
  if(!bi_intptr(argv->cons.car,(intptr_t *)&fp)) return FALSE;
  xbarray_init(&x);
  s = xbarray_fgets(&x, fp);
  if (s == NULL) *result = object_nil;
  else {
    *result = gc_new_barray(x.size--); // remove NUL
    memcpy((*result)->barray.elt, x.elt, x.size);
  }
  xbarray_free(&x);
  return TRUE;
}