// path and files.

/* lisence {{{
   Mulk system.
   Copyright (C) 2009-2018 Ken'ichi Tokuoka. All rights reserved.

   Permission is hereby granted, free of charge, to any person obtaining a copy of
   this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is furnished to do
   so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
   }}} */

#include "std.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#if UNIX_P
#include <unistd.h>
#include <dirent.h>
#include <utime.h>
#define PATH_SEPR '/'
#define PATHS_SEPR ':'
#define xmkdir(x) mkdir(path,0777)
#endif

#if WINDOWS_P
#include <windows.h>
#include <direct.h>
#include <sys/utime.h>
#define utimbuf _utimbuf
#define getcwd _getcwd
#define chdir _chdir
#define xmkdir _mkdir
#define rmdir _rmdir
#define utime _utime
#define PATH_SEPR '\\'
#define PATHS_SEPR ';'
#endif

#include "xbarray.h"
#include "pf.h"

#if WSTAT64_P
#define STATSTR struct __stat64
static int xstat(char *fn, STATSTR *statbuf)
{
  wchar_t wbuf[MAX_STR_LEN];
  if (MultiByteToWideChar(CP_ACP, 0, fn, -1, wbuf, MAX_STR_LEN) == 0) {
    errno = EINVAL;
    return -1;
  }
  return _wstat64(wbuf, statbuf);
}
#else
#define xstat stat
#define STATSTR struct stat
#endif

#if UNIX_P
static int group_member_p(gid_t gid)
{
  int i, size;
  gid_t *gids;
  size = getgroups(0, NULL);
  gids = xmalloc(size * sizeof(gid_t));
  getgroups(size, gids);
  for (i = 0; i < size; i++) {
    if (gid == gids[i]) break;
  }
  xfree(gids);
  return i != size;
}
#endif

int pf_stat(char *fn, struct pf_stat *pf_statbuf)
{
  int result, type, rbit, wbit;
  STATSTR statbuf;
  if (xstat(fn, &statbuf) != 0) {
    if (errno == ENOENT) return PF_NONE;
    return PF_ERROR;
  }
  result = 0;
  type = statbuf.st_mode & S_IFMT;
  if (type == S_IFREG) result = PF_FILE;
  else if (type == S_IFDIR) result = PF_DIR;
  else result = PF_OTHER;
#if UNIX_P
  if (statbuf.st_uid == getuid()) {
    rbit = S_IRUSR;
    wbit = S_IWUSR;
  } else if (group_member_p(statbuf.st_gid)) {
    rbit = S_IRGRP;
    wbit = S_IWGRP;
  } else {
    rbit = S_IROTH;
    wbit = S_IWOTH;
  }
#endif
#if WINDOWS_P|DOS_P
  rbit = S_IREAD;
  wbit = S_IWRITE;
#endif
  if (statbuf.st_mode&rbit) result |= PF_READABLE;
  if (statbuf.st_mode&wbit) result |= PF_WRITABLE;
  if (pf_statbuf != NULL) {
    pf_statbuf->mtime = statbuf.st_mtime;
    pf_statbuf->size = statbuf.st_size;
  }
  return result;
}

static int check(char *fn, int mode)
{
  return (pf_stat(fn, NULL) & mode) == mode;
}

#define READABLEFILE (PF_FILE | PF_READABLE)

static char *find_paths(char *paths, char *name, char *buf)
{
  char *p, *sepr;
  p = paths;
  while ((sepr = strchr(p, PATHS_SEPR)) != NULL) {
    *sepr = '\0';
    xsprintf(buf, "%s/%s", p, name);
    if (check(buf, READABLEFILE)) return buf;
    p = sepr + 1;
  }
  xsprintf(buf, "%s/%s", p, name);
  if (check(buf, READABLEFILE)) return buf;
  return NULL;
}

char *pf_exepath(char *argv0, char *path)
{
  char *p, fn[MAX_STR_LEN], *result;
  struct xbarray paths;
  strcpy(path, argv0);
  p = strrchr(path, PATH_SEPR);
#if !UNIX_P
  {
    char *q;
    q = strrchr(path, '.');
    if (q == NULL || (p != NULL && q < p)) strcat(path, ".exe");
  }
#endif
  if (p != NULL) return path;
#if !UNIX_P
  if (check(path, READABLEFILE)) return path;
#endif
  strcpy(fn, path);
  xbarray_init(&paths);
  xbarray_adds(&paths, getenv("PATH"));
  xbarray_add(&paths, '\0');
  result = find_paths(paths.elt, fn, path);
  xbarray_free(&paths);
  if (result == NULL) xerror("pf_exepath: can not find");
  return result;
}

int pf_utime(char *fn, int64_t mtime)
{
  STATSTR statbuf;
  struct utimbuf utimbuf;
  if (xstat(fn, &statbuf) != 0) return FALSE;
  utimbuf.actime = statbuf.st_atime;
  utimbuf.modtime = mtime;
  if (utime(fn, &utimbuf) == -1) return FALSE;
  return TRUE;
}

char *pf_getcwd(char *buf)
{
  if (getcwd(buf, MAX_STR_LEN) == NULL) xerror("pf_getcwd failed");
  return buf;
}

int pf_readdir(char *path, struct xbarray *dirs)
{
  char *fn;
#if UNIX_P
  {
    DIR *d;
    struct dirent *de;
    if ((d = opendir(path)) == NULL) return FALSE;
    while ((de = readdir(d)) != NULL) {
      fn = de->d_name;
      if (strcmp(fn,".") == 0 || strcmp(fn,"..") == 0) continue;
      xbarray_adds(dirs, fn);
      xbarray_add(dirs, '\n');
    }
    closedir(d);
  }
#endif
#if WINDOWS_P
  {
    WIN32_FIND_DATA data;
    HANDLE h;
    char buf[MAX_STR_LEN];
    DWORD err;
    xsprintf(buf, "%s\\*", path);
    if ((h = FindFirstFile(buf, &data)) == INVALID_HANDLE_VALUE) return FALSE;
    while (TRUE) {
      fn = data.cFileName;
      if (!(strcmp(fn, ".") == 0 || strcmp(fn, "..") == 0)) {
        xbarray_adds(dirs, fn);
        xbarray_add(dirs, '\n');
      }
      if (FindNextFile(h, &data) == 0) {
        err = GetLastError();
        if (err == ERROR_NO_MORE_FILES) break;
        else {
          FindClose(h);
          return FALSE;
        }
      }
    }
    FindClose(h);
  }
#endif
  return TRUE;
}

int pf_mkdir(char *path)
{
  return xmkdir(path) == 0;
}

int pf_remove(char *fn)
{
  if (check(fn, PF_DIR)) return rmdir(fn) == 0;
  return remove(fn) == 0;
}

int pf_chdir(char *dir)
{
  return chdir(dir) == 0;
}
