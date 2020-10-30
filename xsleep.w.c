// xsleep

#include "std.h"
#include "xsleep.h"

void xsleep(double t)
{
  Sleep((int)(t * 1000));
}
