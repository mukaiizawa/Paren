// clip.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int clip_copy(char *p)
{
  char buf[MAX_STR_LEN];
  HGLOBAL gp;
  if (!xmbtombacp(p, buf)) return FALSE;
  if ((gp = GlobalAlloc(GMEM_MOVEABLE, MAX_STR_LEN)) == NULL) return FALSE;
  if ((p = GlobalLock(gp)) == NULL) return FALSE;
  strcpy(p, buf);
  GlobalUnlock(gp);
  if (GetLastError() != NO_ERROR) return FALSE;
  if (!OpenClipboard(NULL)) return FALSE;
  if (!EmptyClipboard()) return FALSE;
  if (SetClipboardData(CF_TEXT, gp) == NULL) return FALSE;
  if (!CloseClipboard()) return FALSE;
  return TRUE;
}

DEFUN(clip_2e_copy)
{
  char *p;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = argv->cons.car;
  if (!bi_cstring(argv, &p)) return FALSE;
  if (!clip_copy(p)) return ip_throw(OSError, clip_failed);
  return TRUE;
}

static int clip_paste(object *result)
{
  void *p;
  char buf[MAX_STR_LEN];
  HANDLE h;
  if (!OpenClipboard(NULL)) return FALSE;
  if ((h = GetClipboardData(CF_UNICODETEXT)) == NULL) return FALSE;
  if ((p = GlobalLock(h)) == NULL) return FALSE;
  if (!CloseClipboard()) return FALSE;
  if (!xwctomb(p, buf)) return FALSE;
  *result = gc_new_mem_from(STRING, buf, strlen(buf));
  return TRUE;
}

DEFUN(clip_2e_paste)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  if (!clip_paste(result)) return ip_throw(OSError, clip_failed);
  return TRUE;
}
