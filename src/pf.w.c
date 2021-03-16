// path and files for windows.

#include "std.h"
#include "pf.h"

#define PATH_SEPR '\\'
#define PATHS_SEPR ';'

int pf_stat(char *fn, struct pf_stat *statbuf)
{
  int mode;
  WCHAR wcbuf[MAX_STR_LEN];
  WIN32_FILE_ATTRIBUTE_DATA attr;
  if (xmbtowc(fn, wcbuf) == 0) return FALSE;
  if (!GetFileAttributesExW(wcbuf, GetFileExInfoStandard, &attr)) {
    switch (GetLastError()) {
      case ERROR_FILE_NOT_FOUND:
      case ERROR_PATH_NOT_FOUND:
      case ERROR_INVALID_NAME:
        return PF_NONE;
      default:
        return PF_ERROR;
    }
  }
  mode = PF_READABLE;
  if (attr.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) mode |= PF_DIR;
  else mode |= PF_FILE;
  if (!(attr.dwFileAttributes & FILE_ATTRIBUTE_READONLY)) mode |= PF_WRITABLE;
  if (statbuf != NULL) {
    statbuf->size = ((int64_t)attr.nFileSizeHigh << 32) | attr.nFileSizeLow;
    statbuf->mtime = (((int64_t)attr.ftLastWriteTime.dwHighDateTime << 32)
        | attr.ftLastWriteTime.dwLowDateTime) / 10000000 - 11644473600;
  }
  return mode;
}

static int mode_p(char *fn, int mode)
{
  return (pf_stat(fn, NULL) & mode) == mode;
}

char *pf_exepath(char *argv0, char *path)
{
  int st;
  WCHAR wcbuf[MAX_STR_LEN];
  st = GetModuleFileNameW(NULL, wcbuf, MAX_STR_LEN);
  if (st >= MAX_STR_LEN) st = 0;
  else st = xwctomb(wcbuf, path);
  if (st == 0) xerror("pf_exepath failed");
  return path;
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
  WCHAR wcbuf[MAX_STR_LEN];
  st = GetCurrentDirectoryW(MAX_STR_LEN, wcbuf);
  if (st > MAX_STR_LEN) st = 0;
  if (st != 0) st = xwctomb(wcbuf, buf);
  if (st == 0) xerror("pf_getcwd failed");
  return buf;
}

FILE *pf_fopen(char *fn, char *mode)
{
  char buf[MAX_STR_LEN];
  if (!xmbtomb(fn, buf)) return FALSE;
  return fopen(buf, mode);
}

int pf_readdir(char *path, struct xbarray *dirs)
{
  char buf[MAX_STR_LEN];
  WCHAR *wcfn, wcbuf[MAX_STR_LEN];
  WIN32_FIND_DATAW data;
  HANDLE h;
  xsprintf(buf, "%s\\*", path);
  if (xmbtowc(buf, wcbuf) == 0) return FALSE;
  if ((h = FindFirstFileW(wcbuf, &data)) == INVALID_HANDLE_VALUE) return FALSE;
  while (TRUE) {
    wcfn = data.cFileName;
    if (xwctomb(wcfn, buf) == 0) return FALSE;
    if (!(strcmp(buf, ".") == 0 || strcmp(buf, "..") == 0)) {
      if (dirs->size != 0) xbarray_add(dirs, '\n');
      xbarray_adds(dirs, buf);
    }
    if (FindNextFileW(h, &data) == 0) {
      switch (GetLastError()) {
        case ERROR_NO_MORE_FILES: FindClose(h); return TRUE;
        default: FindClose(h); return FALSE;
      }
    }
  }
}

int pf_mkdir(char *path)
{
  WCHAR wcbuf[MAX_STR_LEN];
  if (xmbtowc(path, wcbuf) == 0) return FALSE;
  return CreateDirectoryW(wcbuf, NULL);
}

int pf_remove(char *fn)
{
  WCHAR wcbuf[MAX_STR_LEN];
  if (xmbtowc(fn, wcbuf) == 0) return FALSE;
  if (mode_p(fn, PF_DIR)) return RemoveDirectoryW(wcbuf);
  return DeleteFileW(wcbuf);
}

int pf_chdir(char *dir)
{
  WCHAR wcbuf[MAX_STR_LEN];
  if (xmbtowc(dir, wcbuf) == 0) return FALSE;
  return SetCurrentDirectoryW(wcbuf);
}
