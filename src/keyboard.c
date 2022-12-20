// keyboard.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

DEFUN(keyboard_2e_send)
{
  int keycode, dir;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cbyte(argv->cons.car, &keycode)) return FALSE;
  if (!bi_cint(argv->cons.cdr->cons.car, &dir)) return FALSE;
  if (dir == 1) dir = KEYEVENTF_KEYUP;
  else if (dir != 0) return ip_sigerr(ArgumentError, "invalid direction");
  keybd_event(keycode, 0, dir, 0);
  *result = object_nil;
  return TRUE;
}
