// xiconv for unix.

#include "std.h"

void xiconv_init(void)
{
}

int xiconv(int from, int to, void *src, void **result)
{
  *result = src;
  return TRUE;
}
