// path and files for windows.

#include "std.h"
#include "pf.h"
#include <windows.h>

#define PATH_SEPR '\\'
#define PATHS_SEPR ';'

int pf_stat(char *fn, struct pf_stat *statbuf)
{
  int result;
  WIN32_FILE_ATTRIBUTE_DATA attr;
  if (!GetFileAttributesEx(fn, GetFileExInfoStandard, &attr)) {
    switch (GetLastError()) {
      case ERROR_FILE_NOT_FOUND:
      case ERROR_PATH_NOT_FOUND:
      case ERROR_INVALID_NAME:
        return PF_NONE;
      default:
        return PF_ERROR;
    }
  }
  result = PF_READABLE;
  if (attr.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) result |= PF_DIR;
  else result |= PF_FILE;
  if ((attr.dwFileAttributes & FILE_ATTRIBUTE_READONLY) == 0)
    result |= PF_WRITABLE;
  if (statbuf != NULL) {
    statbuf->size = ((int64_t)attr.nFileSizeHigh << 32) | attr.nFileSizeLow;
    statbuf->mtime = (((int64_t)attr.ftLastWriteTime.dwHighDateTime << 32)
        | attr.ftLastWriteTime.dwLowDateTime) / 10000000 - 11644473600;
  }
  return result;
}

static int check(char *fn, int mode)
{
  return (pf_stat(fn, NULL) & mode) == mode;
}

static char *find_paths(char *paths, char *name, char *buf)
{
  char *p, *sepr;
  p = paths;
  while ((sepr = strchr(p, PATHS_SEPR)) != NULL) {
    *sepr = '\0';
    xsprintf(buf, "%s/%s", p, name);
    if (check(buf, PF_READABLEFILE)) return buf;
    p = sepr + 1;
  }
  xsprintf(buf, "%s/%s", p, name);
  if (check(buf, PF_READABLEFILE)) return buf;
  return NULL;
}

char *pf_exepath(char *argv0, char *path)
{
  char *p, *q, fn[MAX_STR_LEN], *result;
  struct xbarray paths;
  strcpy(path, argv0);
  p = strrchr(path, PATH_SEPR);
  q = strrchr(path, '.');
  if (q == NULL || (p != NULL && q < p)) strcat(path,".exe");
  if (p != NULL) return path;
  if (check(path, PF_READABLEFILE)) return path;
  strcpy(fn, path);
  xbarray_init(&paths);
  xbarray_adds(&paths,getenv("PATH"));
  xbarray_add(&paths, '\0');
  result = find_paths(paths.elt, fn, path);
  xbarray_free(&paths);
  if (result == NULL) xerror("pf_exepath: can not find");
  return result;
}

int pf_utime(char *fn, int64_t mtime)
{
  HANDLE hFile;
  FILETIME ft;
  mtime = mtime * 10000000 + 116444736000000000;
  ft.dwLowDateTime = mtime & 0xffffffff;
  ft.dwHighDateTime = mtime >> 32;
  hFile = CreateFile(fn, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, NULL);
  if (hFile == INVALID_HANDLE_VALUE) return FALSE;
  if (!SetFileTime(hFile, NULL, NULL, &ft)) return FALSE;
  CloseHandle(hFile);
  return TRUE;
}

char *pf_getcwd(char *buf)
{
  int st;
  st = GetCurrentDirectory(MAX_STR_LEN, buf);
  if (st > MAX_STR_LEN) st = 0;
  if (!st) xerror("pf_getcwd failed");
  return buf;
}

int pf_readdir(char *path, struct xbarray *dirs)
{
  char *fn, buf[MAX_STR_LEN];
  WIN32_FIND_DATA data;
  HANDLE h;
  DWORD err;
  xsprintf(buf, "%s\\*", path);
  if ((h = FindFirstFile(buf, &data)) == INVALID_HANDLE_VALUE) return FALSE;
  while (TRUE) {
    fn = data.cFileName;
    if (!(strcmp(fn,".") == 0 || strcmp(fn,"..") == 0)) {
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
  return TRUE;
}

int pf_mkdir(char *path)
{
  return CreateDirectory(path, NULL);
}

int pf_remove(char *fn)
{
  int st;
  if (check(fn, PF_DIR)) st = RemoveDirectory(fn);
  else st = DeleteFile(fn);
  return st;
}

int pf_chdir(char *dir)
{
  return SetCurrentDirectory(dir);
}
