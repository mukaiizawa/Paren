// xsleep

#include "std.h"
#include "xsleep.h"
#include <windows.h>

void xsleep(double t)
{
  Sleep((int)(t * 1000));
}
