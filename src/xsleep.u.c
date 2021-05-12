// xsleep for unix.

#include "std.h"
#include "xsleep.h"

void xsleep(double t)
{
  struct timespec ts, rem;
  ts.tv_sec = t;
  ts.tv_nsec = (t - ts.tv_sec) * 1000000000;
  nanosleep(&ts, &rem);
}
