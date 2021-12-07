// xiconv for windows.

#include "std.h"
#include "xiconv.h"

static int mb_buf_size;
static char *mb_buf;

static int wc_buf_size;
static LPWSTR wc_buf;

void xiconv_init(void)
{
  mb_buf_size = wc_buf_size = 8;
  mb_buf = xmalloc(sizeof(char *) * mb_buf_size);
  wc_buf = xmalloc(sizeof(LPWSTR) * wc_buf_size);
}

int xiconv_wc2mb(int enc, LPWSTR src, char **result)
{
  int size;
  size = WideCharToMultiByte(enc, 0, src, -1, NULL, 0, NULL, NULL);
  if (size == 0) return FALSE;
  while (size >= mb_buf_size) {
    mb_buf_size *= 2;
    mb_buf = xrealloc(mb_buf, sizeof(char *) * mb_buf_size);
  }
  *result = mb_buf;
  return WideCharToMultiByte(enc, 0, src, -1, mb_buf, size, NULL, NULL);
}

int xiconv_mb2wc(int enc, char *src, LPWSTR *result)
{
  int size;
  size = MultiByteToWideChar(enc, 0, src, -1, NULL, 0);
  if (size == 0) return FALSE;
  while (size >= wc_buf_size) {
    wc_buf_size *= 2;
    wc_buf = xrealloc(wc_buf, sizeof(LPWSTR) * wc_buf_size);
  }
  *result = wc_buf;
  return MultiByteToWideChar(enc, 0, src, -1, wc_buf, size);
}

int xiconv(int from, int to, char *src, char **result)
{
  LPWSTR p;
  return xiconv_mb2wc(from, src, &p) && xiconv_wc2mb(to, p, result);
}
