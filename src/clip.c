// clip.

#include "std.h"
#include "xiconv.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int clip_copy(char *text)
{
  char *ansi_text;
  HGLOBAL gp;
  if (!xiconv(XICONV_UTF8, XICONV_ANSI, text, &ansi_text)) return FALSE;
  if ((gp = GlobalAlloc(GMEM_MOVEABLE, strlen(ansi_text) + 1)) == NULL) return FALSE;
  if ((text = GlobalLock(gp)) == NULL) return FALSE;
  strcpy(text, ansi_text);
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
  char *text;
  void *ansi_text;
  HANDLE h;
  if (!OpenClipboard(NULL)) return FALSE;
  if ((h = GetClipboardData(CF_TEXT)) == NULL) return FALSE;
  if ((ansi_text = GlobalLock(h)) == NULL) return FALSE;
  if (!CloseClipboard()) return FALSE;
  if (!xiconv(XICONV_ANSI, XICONV_UTF8, ansi_text, &text)) return FALSE;
  *result = gc_new_mem_from_cstr(STRING, text);
  return TRUE;
}

DEFUN(clip_2e_paste)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  if (!clip_paste(result)) return ip_throw(OSError, clip_failed);
  return TRUE;
}
