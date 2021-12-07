// path and files for windows.

#include "std.h"
#include "xiconv.h"
#include "pf.h"

#define PATH_SEPR '\\'
#define PATHS_SEPR ';'

#define WIN_TICK 10000000
#define WIN_OFFSET 11644473600
#define win2unix(t) ((t / WIN_TICK) - WIN_OFFSET)
#define unix2win(t) ((t + WIN_OFFSET) * WIN_TICK)

static int64_t int64(DWORD high, DWORD low)
{
  return ((int64_t)high << 32) | low;
}

int pf_stat(char *fn, struct pf_stat *statbuf)
{
  int mode;
  LPWSTR wc_fn;
  WIN32_FILE_ATTRIBUTE_DATA attr;
  if (!xiconv_mb2wc(XICONV_UTF8, fn, &wc_fn)) return FALSE;
  if (!GetFileAttributesExW(wc_fn, GetFileExInfoStandard, &attr)) {
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
  else mode |= PF_REGF;
  if (!(attr.dwFileAttributes & FILE_ATTRIBUTE_READONLY)) mode |= PF_WRITABLE;
  if (statbuf != NULL) {
    statbuf->size = int64(attr.nFileSizeHigh, attr.nFileSizeLow);
    statbuf->mtime = win2unix(int64(attr.ftLastWriteTime.dwHighDateTime, attr.ftLastWriteTime.dwLowDateTime));
  }
  return mode;
}

static int mode_p(char *fn, int mode)
{
  return (pf_stat(fn, NULL) & mode) == mode;
}

char *pf_exepath(char *argv0, char *path)
{
  char *mb_path;
  WCHAR wc_buf[MAX_STR_LEN];
  if (GetModuleFileNameW(NULL, wc_buf, MAX_STR_LEN) >= MAX_STR_LEN
      || !xiconv_wc2mb(XICONV_UTF8, wc_buf, &mb_path))
    xerror("pf_exepath/failed");
  strcpy(path, mb_path);
  return path;
}

int pf_utime(char *fn, int64_t mtime)
{
  HANDLE hFile;
  FILETIME ft;
  mtime = unix2win(mtime);
  ft.dwLowDateTime = mtime & 0xffffffff;
  ft.dwHighDateTime = mtime >> 32;
  hFile = CreateFile(fn, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (hFile == INVALID_HANDLE_VALUE) return FALSE;
  if (!SetFileTime(hFile, NULL, NULL, &ft)) return FALSE;
  CloseHandle(hFile);
  return TRUE;
}

char *pf_getcwd(char *buf)
{
  char *mb_cwd;
  WCHAR wc_buf[MAX_STR_LEN];
  if (GetCurrentDirectoryW(MAX_STR_LEN, wc_buf) >= MAX_STR_LEN
      || !xiconv_wc2mb(XICONV_UTF8, wc_buf, &mb_cwd))
    xerror("pf_getcwd/failed");
  strcpy(buf, mb_cwd);
  return buf;
}

FILE *pf_fopen(char *fn, char *mode)
{
  char *mb_fn;
  if (!xiconv(XICONV_UTF8, XICONV_ANSI, fn, &mb_fn)) return FALSE;
  return fopen(mb_fn, mode);
}

int pf_readdir(char *path, struct xbarray *dirs)
{
  char *mb_file, mb_buf[MAX_STR_LEN];
  LPWSTR wc_path;
  WIN32_FIND_DATAW data;
  HANDLE h;
  xsprintf(mb_buf, "%s\\*", path);
  if (!xiconv_mb2wc(XICONV_UTF8, mb_buf, &wc_path)) return FALSE;
  if ((h = FindFirstFileW(wc_path, &data)) == INVALID_HANDLE_VALUE) return FALSE;
  while (TRUE) {
    if (!xiconv_wc2mb(XICONV_UTF8, data.cFileName, &mb_file)) return FALSE;
    if (!(strcmp(mb_file, ".") == 0 || strcmp(mb_file, "..") == 0)) {
      if (dirs->size != 0) xbarray_add(dirs, '\n');
      xbarray_adds(dirs, mb_file);
    }
    if (FindNextFileW(h, &data) == 0) {
      switch (GetLastError()) {
        case ERROR_NO_MORE_FILES:
          FindClose(h);
          return TRUE;
        default:
          FindClose(h);
          return FALSE;
      }
    }
  }
}

int pf_mkdir(char *path)
{
  LPWSTR wc_path;
  if (!xiconv_mb2wc(XICONV_UTF8, path, &wc_path)) return FALSE;
  return CreateDirectoryW(wc_path, NULL);
}

int pf_remove(char *fn)
{
  LPWSTR wc_fn;
  if (!xiconv_mb2wc(XICONV_UTF8, fn, &wc_fn)) return FALSE;
  if (mode_p(fn, PF_DIR)) return RemoveDirectoryW(wc_fn);
  return DeleteFileW(wc_fn);
}

int pf_chdir(char *dir)
{
  LPWSTR wc_dir;
  return xiconv_mb2wc(XICONV_UTF8, dir, &wc_dir) && SetCurrentDirectoryW(wc_dir);
}
