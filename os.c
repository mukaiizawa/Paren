// OS class

#include "std.h"

#include <time.h>
#include <string.h>
#include <errno.h>

#include "xsleep.h"
#include "xbarray.h"
#include "xarray.h"
#include "splay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "pf.h"
#include "ip.h"

DEFUN(os_fp)
{
  int fd;
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_sint(argv->cons.car, &fd)) {
    ip_mark_error("illegal file discripter");
    return FALSE;
  }
  switch (fd) {
    case 0: fp = stdin; break;
    case 1: fp = stdout; break;
    case 2: fp = stderr; break;
    default: ip_mark_error("unknown file discripter"); return FALSE;
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

DEFUN(os_fopen)
{
  char *fn;
  int mode;
  FILE *fp;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if ((fn = bi_string(argv)) == NULL) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &mode)) return FALSE;
  if (0 > mode || mode >= sizeof(mode_table) / sizeof(char *)) return FALSE;
  if ((fp = fopen(fn, mode_table[mode])) == NULL) return FALSE;
  *result = gc_new_xint((intptr_t) fp);
  return TRUE;
}

DEFUN(os_fgetc)
{
  int ch;
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_fp(argv->cons.car, &fp)) return FALSE;
  ch = fgetc(fp);
  if (ch == EOF && ferror(fp)) {
    clearerr(fp);
    return FALSE;
  }
  *result = gc_new_xint(ch);
  return TRUE;
}

DEFUN(os_fputc)
{
  int byte;
  FILE *fp;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_sint(argv->cons.car, &byte)) return FALSE;
  if (!byte_p(byte)) return FALSE;
  if (!bi_intptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  if (fputc((int)byte, fp) == EOF) return FALSE;
  return TRUE;
}

DEFUN(os_fgets)
{
  char *s;
  FILE *fp;
  struct xbarray x;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_intptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  xbarray_init(&x);
  s = xbarray_fgets(&x, fp);
  if (s == NULL) *result = object_nil;
  else {
    *result = gc_new_bytes(STRING, --x.size);    // remove last NUL
    memcpy((*result)->bytes.elt, x.elt, x.size);
  }
  xbarray_free(&x);
  return TRUE;
}

DEFUN(os_fread)
{
  object o;
  int from, size;
  FILE *fp;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_arg_type(argv->cons.car, BYTES, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!(0 <= from && from + size <= o->bytes.size)) return FALSE;
  if (!bi_intptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  size = fread(o->bytes.elt + from, 1, size, fp);
  if (size == 0 && ferror(fp)) {
    clearerr(fp);
    return FALSE;
  }
  *result = gc_new_xint(size);
  return TRUE;
}

DEFUN(os_fwrite)
{
  object o;
  int from, size;
  FILE *fp;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_arg_bytes(argv->cons.car, &o)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!(0 <= from && from + size <= o->bytes.size)) return FALSE;
  if (!bi_intptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  size = fwrite(o->bytes.elt + from, 1, size, fp);
  if (size == 0 && ferror(fp)) {
    clearerr(fp);
    return FALSE;
  }
  *result = gc_new_xint(size);
  return TRUE;
}

DEFUN(os_fseek)
{
  int off;
  FILE *fp;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_intptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &off)) return FALSE;
  *result = object_nil;
  if (off == -1) return fseek(fp, 0, SEEK_END) == 0;
  return fseek(fp, off, SEEK_SET) == 0;
}

DEFUN(os_ftell)
{
  int pos;
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_intptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  if ((pos = ftell(fp)) == -1) return FALSE;
  *result = gc_new_xint(pos);
  return TRUE;
}

DEFUN(os_fclose)
{
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_intptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  fclose(fp);
  *result = object_nil;
  return TRUE;
}

DEFUN(os_stat)
{
  int type;
  char *fn;
  struct pf_stat statbuf;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if ((fn = bi_string(argv)) == NULL) return FALSE;
  type = pf_stat(fn, &statbuf);
  if (type == PF_ERROR) return FALSE;
  if (type == PF_NONE) {
    *result = object_nil;
    return TRUE;
  }
  *result = gc_new_array(3);
  (*result)->array.elt[0] = gc_new_xint(type);
  (*result)->array.elt[1] = gc_new_xint(statbuf.size);
  (*result)->array.elt[2] = gc_new_xint(statbuf.mtime);
  return TRUE;
}

DEFUN(os_utime)
{
  char *fn;
  int64_t tv;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if ((fn = bi_string(argv)) == NULL) return FALSE;
  if (!bi_int64(argv->cons.cdr->cons.car, &tv)) return FALSE;
  *result = object_nil;
  return pf_utime(fn, tv);
}

DEFUN(os_getcwd)
{
  char buf[MAX_STR_LEN];
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  pf_getcwd(buf);
  *result = gc_new_bytes_from(STRING, buf, strlen(buf));
  return TRUE;
}

DEFUN(os_chdir)
{
  char *fn;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if ((fn = bi_string(argv)) == NULL) return FALSE;
  *result = object_nil;
  return pf_chdir(fn);
}

DEFUN(os_readdir)
{
  char *path;
  struct xbarray dirs;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if ((path = bi_string(argv)) == NULL) return FALSE;
  xbarray_init(&dirs);
  if (!pf_readdir(path, &dirs)) return FALSE;
  *result = gc_new_bytes_from(STRING, dirs.elt, dirs.size);
  xbarray_free(&dirs);
  return TRUE;
}

DEFUN(os_remove)
{
  char *fn;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if ((fn = bi_string(argv)) == NULL) return FALSE;
  *result = object_nil;
  return pf_remove(fn);
}

DEFUN(os_mkdir)
{
  char *path;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if ((path = bi_string(argv)) == NULL) return FALSE;
  *result = object_nil;
  return pf_mkdir(path);
}

DEFUN(os_rename)
{
  char *src_dst[2];
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_strings(2, argv, src_dst)) return FALSE;
  *result = object_nil;
  return rename(src_dst[0], src_dst[1]) == 0;
}

DEFUN(os_time)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  *result = gc_new_xint(time(NULL));
  return TRUE;
}

DEFUN(os_clock)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  *result = gc_new_xfloat((double)clock() / CLOCKS_PER_SEC);
  return TRUE;
}

DEFUN(os_sleep)
{
  double t;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_double(argv->cons.car, &t)) return FALSE;
  xsleep(t);
  *result = object_nil;
  return TRUE;
}

static int daysec(struct tm *t)
{
  return (t->tm_hour * 60 + t->tm_min) * 60 + t->tm_sec;
}

DEFUN(os_timediff)
{
  int off;
  time_t now;
  struct tm gtm, ltm;
  now = time(NULL);
  gtm = *gmtime(&now);
  ltm = *localtime(&now);
  if (gtm.tm_yday == ltm.tm_yday) off = 0;
  else if (gtm.tm_year == ltm.tm_year + 1) off = -1;
  else if (gtm.tm_yday == ltm.tm_yday + 1) off = -1;
  else off = 1;
  *result = gc_new_xint(daysec(&ltm) + off * 24 * 60 * 60 - daysec(&gtm));
  return TRUE;
}

DEFUN(os_system)
{
  char *s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if ((s = bi_string(argv)) == NULL) return FALSE;
  *result = sint(system(s));
  return TRUE;
}

DEFUN(os_getenv)
{
  char *s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if ((s = bi_string(argv)) == NULL) return FALSE;
  if ((s = getenv(s)) == NULL) *result = object_nil;
  else *result = gc_new_bytes_from(STRING, s, strlen(s));
  return TRUE;
}

DEFUN(os_putenv)
{
  char *kv[2];
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_strings(2, argv, kv)) return FALSE;
  *strchr(kv[0], '\0') = '=';
  *result = object_nil;
  return putenv(xstrdup(kv[0])) == 0;
}
