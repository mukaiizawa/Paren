// os module.

#include "std.h"
#include "xsleep.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "pf.h"
#include "ip.h"

DEFUN(fp)
{
  int fd;
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cint(argv->cons.car, &fd)) return FALSE;
  switch (fd) {
    case 0: fp = stdin; break;
    case 1: fp = stdout; break;
    case 2: fp = stderr; break;
    default: return ip_throw(OSError, fp_failed);
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
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cstring(argv, &fn)) return FALSE;
  if (!bi_cpint(argv->cons.cdr->cons.car, &mode)) return FALSE;
  if (mode >= sizeof(mode_table) / sizeof(char *))
    return ip_throw(ArgumentError, invalid_args);
  if ((fp = pf_fopen(fn, mode_table[mode])) == NULL)
    return ip_throw(OSError, fopen_failed);
  *result = gc_new_xint((intptr_t)fp);
  return TRUE;
}

DEFUN(fgetc)
{
  int ch;
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  ch = fgetc(fp);
  if (ch == EOF && ferror(fp)) {
    clearerr(fp);
    return ip_throw(OSError, fgetc_failed);
  }
  *result = gc_new_xint(ch);
  return TRUE;
}

DEFUN(fputc)
{
  int byte;
  FILE *fp;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  *result = argv->cons.car;
  if (!bi_cbyte(argv->cons.car, &byte)) return FALSE;
  if (!bi_cintptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  if (fputc(byte, fp) == EOF)
    return ip_throw(OSError, fputc_failed);
  return TRUE;
}

DEFUN(fgets)
{
  char *s;
  FILE *fp;
  struct xbarray x;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  xbarray_init(&x);
  s = xbarray_fgets(&x, fp);
  if (s == NULL) *result = object_nil;
  else {
    *result = gc_new_mem(STRING, --x.size);    // remove last NUL
    memcpy((*result)->mem.elt, x.elt, x.size);
  }
  xbarray_free(&x);
  return TRUE;
}

DEFUN(fread)
{
  int from, size;
  FILE *fp;
  object o;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_bytes(argv->cons.car, &o)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_cint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!bi_range(0, from + size, o->mem.size)) return FALSE;
  if (!bi_cintptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  size = fread(o->mem.elt + from, 1, size, fp);
  if (size == 0 && ferror(fp)) {
    clearerr(fp);
    return ip_throw(OSError, fread_failed);
  }
  *result = gc_new_xint(size);
  return TRUE;
}

DEFUN(fwrite)
{
  int from, size;
  FILE *fp;
  object o;
  if (!bi_argc_range(argc, 4, 4)) return FALSE;
  if (!bi_bytes_like(argv->cons.car, &o)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &from)) return FALSE;
  if (!bi_cpint((argv = argv->cons.cdr)->cons.car, &size)) return FALSE;
  if (!bi_range(0, from + size, o->mem.size)) return FALSE;
  if (!bi_cintptr(argv->cons.cdr->cons.car, (intptr_t *)&fp)) return FALSE;
  size = fwrite(o->mem.elt + from, 1, size, fp);
  if (size == 0 && ferror(fp)) {
    clearerr(fp);
    return ip_throw(OSError, fwrite_failed);
  }
  *result = gc_new_xint(size);
  return TRUE;
}

DEFUN(fseek)
{
  int st, off;
  FILE *fp;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  if (!bi_cint((argv = argv->cons.cdr)->cons.car, &off)) return FALSE;
  *result = object_nil;
  if (off == -1) st = fseek(fp, 0, SEEK_END);
  else st = fseek(fp, off, SEEK_SET);
  if (st != 0) return ip_throw(OSError, fseek_failed);
  return TRUE;
}

DEFUN(ftell)
{
  int pos;
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  if ((pos = ftell(fp)) == -1) return ip_throw(OSError, ftell_failed);
  *result = gc_new_xint(pos);
  return TRUE;
}

DEFUN(fclose)
{
  FILE *fp;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cintptr(argv->cons.car, (intptr_t *)&fp)) return FALSE;
  fclose(fp);
  *result = object_nil;
  return TRUE;
}

DEFUN(stat)
{
  int mode;
  char *fn;
  struct pf_stat statbuf;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &fn)) return FALSE;
  mode = pf_stat(fn, &statbuf);
  if (mode == PF_ERROR) return ip_throw(OSError, stat_failed);
  if (mode == PF_NONE) {
    *result = object_nil;
    return TRUE;
  }
  *result = gc_new_array(3);
  (*result)->array.elt[0] = gc_new_xint(mode);
  (*result)->array.elt[1] = gc_new_xint(statbuf.size);
  (*result)->array.elt[2] = gc_new_xint(statbuf.mtime);
  return TRUE;
}

DEFUN(utime)
{
  char *fn;
  int64_t tv;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cstring(argv, &fn)) return FALSE;
  if (!bi_cint64(argv->cons.cdr->cons.car, &tv))
    return ip_throw(ArgumentError, expected_integer);
  *result = object_nil;
  return pf_utime(fn, tv);
}

DEFUN(getcwd)
{
  char buf[MAX_STR_LEN];
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  pf_getcwd(buf);
  *result = gc_new_mem_from(STRING, buf, strlen(buf));
  return TRUE;
}

DEFUN(chdir)
{
  char *fn;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &fn)) return FALSE;
  *result = object_nil;
  return pf_chdir(fn);
}

DEFUN(readdir)
{
  char *path;
  struct xbarray dirs;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &path)) return FALSE;
  xbarray_init(&dirs);
  if (!pf_readdir(path, &dirs))
    return ip_throw(OSError, readdir_failed);
  *result = gc_new_mem_from(STRING, dirs.elt, dirs.size);
  xbarray_free(&dirs);
  return TRUE;
}

DEFUN(remove)
{
  char *fn;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &fn)) return FALSE;
  *result = object_nil;
  return pf_remove(fn);
}

DEFUN(mkdir)
{
  char *path;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &path)) return FALSE;
  *result = object_nil;
  return pf_mkdir(path);
}

DEFUN(rename)
{
  char *src_dst[2];
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cstrings(2, argv, src_dst)) return FALSE;
  *result = object_nil;
  return rename(src_dst[0], src_dst[1]) == 0;
}

DEFUN(time)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  *result = gc_new_xint(time(NULL));
  return TRUE;
}

DEFUN(clock)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  *result = gc_new_xfloat((double)clock() / CLOCKS_PER_SEC);
  return TRUE;
}

DEFUN(sleep)
{
  double t;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cdouble(argv->cons.car, &t)) return FALSE;
  xsleep(t);
  *result = object_nil;
  return TRUE;
}

static int daysec(struct tm *t)
{
  return (t->tm_hour * 60 + t->tm_min) * 60 + t->tm_sec;
}

DEFUN(utcoffset)
{
  int off;
  time_t now;
  struct tm gtm, ltm;
  now = time(NULL);
  gtm = *gmtime(&now);
  ltm = *localtime(&now);
  if (gtm.tm_yday == ltm.tm_yday) off = 0;
  else if (gtm.tm_yday == ltm.tm_yday + 1) off = -1;
  else off = 1;
  *result = gc_new_xint(daysec(&ltm) + off * 24 * 60 * 60 - daysec(&gtm));
  return TRUE;
}

DEFUN(system)
{
  char *s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &s)) return FALSE;
  *result = gc_new_xint(system(s));
  return TRUE;
}

DEFUN(getenv)
{
  char *s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &s)) return FALSE;
  if ((s = getenv(s)) == NULL) *result = object_nil;
  else *result = gc_new_mem_from(STRING, s, strlen(s));
  return TRUE;
}

DEFUN(putenv)
{
  char *kv[2];
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cstrings(2, argv, kv)) return FALSE;
  *strchr(kv[0], '\0') = '=';
  *result = object_nil;
  return putenv(xstrdup(kv[0])) == 0;
}
