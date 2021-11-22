// mouse.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static object point_to_object(POINT *p)
{
  return gc_new_cons(gc_new_xint(p->x), gc_new_cons(gc_new_xint(p->y), object_nil));
}

static int object_to_point(object o, POINT *p)
{
  int i;
  object q;
  if (!bi_cons(o, &q)) return FALSE;
  if (!bi_cint(o->cons.car, &i)) return FALSE;
  p->x = i;
  if (!bi_cons((o = o->cons.cdr), &q)) return FALSE;
  if (!bi_cint(o->cons.car, &i)) return FALSE;
  p->y = i;
  return TRUE;
}

DEFUN(mouse_2e_position)
{
  POINT p;
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  GetCursorPos(&p);
  *result = point_to_object(&p);
  return TRUE;
}

DEFUN(mouse_2e_move)
{
  POINT p;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!object_to_point(argv->cons.car, &p)) return FALSE;
  SetCursorPos(p.x, p.y);
  *result = argv->cons.car;
  return TRUE;
}

static const int BTN_KIND = 3;
static const int DIR_KIND = 2;

static int flg_table[] = {
  MOUSEEVENTF_LEFTDOWN, MOUSEEVENTF_LEFTUP,
  MOUSEEVENTF_MIDDLEDOWN, MOUSEEVENTF_MIDDLEUP,
  MOUSEEVENTF_RIGHTDOWN, MOUSEEVENTF_RIGHTUP,
};

DEFUN(mouse_2e_fire)
{
  int btn, dir;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cint(argv->cons.car, &btn)) return FALSE;
  if (!bi_range(0, btn, BTN_KIND)) return FALSE;
  if (!bi_cint(argv->cons.cdr->cons.car, &dir)) return FALSE;
  if (!bi_range(0, dir, DIR_KIND)) return FALSE;
  mouse_event(flg_table[btn * DIR_KIND + dir], 0, 0, 0, 0);    // at the current coordinates.
  *result = object_nil;
  return TRUE;
}
