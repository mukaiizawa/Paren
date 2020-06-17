// xsleep

#include "std.h"
#include "xsleep.h"

#if WINDOWS_P
#include <windows.h>
#endif

#if UNIX_P
#include <time.h>
#endif

void xsleep(double t)
{
#if WINDOWS_P
  Sleep((int)(t*1000));
#endif
#if UNIX_P
  struct timespec ts,rem;
  ts.tv_sec=t;
  ts.tv_nsec=(t-ts.tv_sec)*1000000000;
  nanosleep(&ts,&rem);
#endif
}
