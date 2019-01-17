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

PRIM(fp)
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

PRIM(fopen)
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
  *result = gc_new_xint((intptr_t) fp);
  return TRUE;
}

PRIM(fgetc)
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

PRIM(fputc)
{
  int64_t byte;
  FILE *fp;
  if (argc != 2) return FALSE;
  if (!bi_int64(argv->cons.car, &byte) || !byte_range_p(byte)) return FALSE;
  if (!bi_intptr(argv->cons.cdr->cons.car,(intptr_t *)&fp)) return FALSE;
  if (fputc((int)byte, fp) == EOF) return FALSE;
  return TRUE;
}

PRIM(fgets)
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

PRIM(fread)
{
  object o;
  int64_t from, size;
  FILE *fp;
  if (argc != 4) return FALSE;
  if (!typep((o = argv->cons.car), BARRAY)) return FALSE;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!(0 <= from && from + size <= o->barray.size)) return FALSE;
  if (!bi_intptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  size = fread(o->barray.elt + from, 1, size, fp);
  if (size == 0 && ferror(fp)) {
    clearerr(fp);
    return FALSE;
  }
  *result = gc_new_xint(size);
  return TRUE;
}

PRIM(fwrite)
{
  object o;
  int64_t from, size;
  FILE *fp;
  if (argc != 4) return FALSE;
  if (!typep((o = argv->cons.car), BARRAY)) return FALSE;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!(0 <= from && from + size <= o->barray.size)) return FALSE;
  if (!bi_intptr(argv->cons.cdr->cons.car,(intptr_t *)&fp)) return FALSE;
  size = fwrite(o->barray.elt + from, 1, size, fp);
  if (size == 0 && ferror(fp)) {
    clearerr(fp);
    return FALSE;
  }
  *result = gc_new_xint(size);
  return TRUE;
}

PRIM(fseek)
{
  int64_t off;
  FILE *fp;
  if (argc != 2) return FALSE;
  if (!bi_intptr(argv->cons.car,(intptr_t *)&fp)) return FALSE;
  if (!bi_int64((argv = argv->cons.cdr)->cons.car, &off)) return FALSE;
  if (off == -1) return fseek(fp, 0, SEEK_END) == 0;
  return fseek(fp, off, SEEK_SET)==0;
}

PRIM(ftell)
{
  int64_t pos;
  FILE *fp;
  if (argc != 1) return FALSE;
  if (!bi_intptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  if ((pos = ftell(fp)) == -1) return FALSE;
  *result = gc_new_xint(pos);
  return TRUE;
}

PRIM(fclose)
{
  FILE *fp;
  if (argc != 1) return FALSE;
  if (!bi_intptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  fclose(fp);
  return TRUE;
}
