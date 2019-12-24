// OS class

#include "std.h"

#include <time.h>
#include <string.h>
#include <errno.h>

#include "xbarray.h"
#include "xarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "pf.h"
#include "ip.h"

static int ensure_file_pointer(object o, FILE **fp)
{
  if (!bi_intptr(o, (intptr_t *)fp)) {
    ip_mark_exception("illegal file pointer");
    return FALSE;
  }
  return TRUE;
}

DEFUN(fp)
{
  int fd;
  FILE *fp;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!bi_int(argv->cons.car, &fd)) {
    ip_mark_exception("illegal file discripter");
    return FALSE;
  }
  switch (fd) {
    case 0: fp = stdin; break;
    case 1: fp = stdout; break;
    case 2: fp = stderr; break;
    default: ip_mark_exception("unknown file discripter"); return FALSE;
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

DEFUN(fopen)
{
  char *fn;
  int mode;
  FILE *fp;
  object ofn;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!typep(ofn = argv->cons.car, STRING)) {
    ip_mark_exception("illegal file name");
    return FALSE;
  }
  fn = ofn->barray.elt;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &mode)
      || (0 > mode || mode >= sizeof(mode_table) / sizeof(char *)))
    ip_mark_exception("illegal open mode");
  else if ((fp = fopen(fn, mode_table[mode])) == NULL)
    ip_mark_exception("cannot open file");
  else {
    *result = gc_new_xint((intptr_t) fp);
    return TRUE;
  }
  return FALSE;
}

DEFUN(fgetc)
{
  int ch;
  FILE *fp;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!ensure_file_pointer(argv->cons.car, &fp)) return FALSE;
  ch = fgetc(fp);
  if (ch == EOF && ferror(fp)) {
    clearerr(fp);
    return FALSE;
  }
  *result = gc_new_xint(ch);
  return TRUE;
}

DEFUN(fputc)
{
  int byte;
  FILE *fp;
  if (argc != 2) return FALSE;
  if (!bi_int(argv->cons.car, &byte) || !byte_range_p(byte)) return FALSE;
  if (!bi_intptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  if (fputc((int)byte, fp) == EOF) return FALSE;
  return TRUE;
}

DEFUN(fgets)
{
  char *s;
  FILE *fp;
  struct xbarray x;
  if (argc != 1) return FALSE;
  if(!bi_intptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  xbarray_init(&x);
  s = xbarray_fgets(&x, fp);
  if (s == NULL) *result = object_nil;
  else {
    *result = gc_new_barray(STRING, --x.size);// remove NUL
    memcpy((*result)->barray.elt, x.elt, x.size);
  }
  xbarray_free(&x);
  return TRUE;
}

DEFUN(fread)
{
  object o;
  int from, size;
  FILE *fp;
  if (argc != 4) return FALSE;
  if (!typep((o = argv->cons.car), BARRAY)) return FALSE;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
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

DEFUN(fwrite)
{
  object o;
  int from, size;
  FILE *fp;
  if (argc != 4) return FALSE;
  if (!(typep((o = argv->cons.car), BARRAY) || typep(o, STRING))) return FALSE;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!(0 <= from && from + size <= o->barray.size)) return FALSE;
  if (!bi_intptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  size = fwrite(o->barray.elt + from, 1, size, fp);
  if (size == 0 && ferror(fp)) {
    clearerr(fp);
    return FALSE;
  }
  *result = gc_new_xint(size);
  return TRUE;
}

DEFUN(fseek)
{
  int off;
  FILE *fp;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  if (!bi_intptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  if (!bi_int((argv = argv->cons.cdr)->cons.car, &off)) return FALSE;
  if (off == -1) return fseek(fp, 0, SEEK_END) == 0;
  return fseek(fp, off, SEEK_SET)==0;
}

DEFUN(ftell)
{
  int pos;
  FILE *fp;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!bi_intptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  if ((pos = ftell(fp)) == -1) return FALSE;
  *result = gc_new_xint(pos);
  return TRUE;
}

DEFUN(fclose)
{
  FILE *fp;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!bi_intptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  fclose(fp);
  *result = object_true;
  return TRUE;
}

DEFUN(clock)
{
  if (!ip_ensure_arguments(argc, FALSE, FALSE)) return FALSE;
  *result = gc_new_xfloat((double)clock() / CLOCKS_PER_SEC);
  return TRUE;
}

DEFUN(milli_time)
{
  if (!ip_ensure_arguments(argc, FALSE, FALSE)) return FALSE;
  *result = gc_new_xint(time(NULL));
  return TRUE;
}
