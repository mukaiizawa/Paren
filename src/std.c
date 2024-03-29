// xc std.

/* lisence {{{
Mulk system.
Copyright (C) 2009-2020 Ken'ichi Tokuoka. All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
}}} */

#include "std.h"

int xbitc (int bits) {
  bits = (bits & 0x55555555) + (bits >> 1 & 0x55555555);
  bits = (bits & 0x33333333) + (bits >> 2 & 0x33333333);
  bits = (bits & 0x0f0f0f0f) + (bits >> 4 & 0x0f0f0f0f);
  bits = (bits & 0x00ff00ff) + (bits >> 8 & 0x00ff00ff);
  return (bits & 0x0000ffff) + (bits >> 16 & 0x0000ffff);
}

void xvsprintf(char *buf, char *fmt, va_list va)
{
  if (vsnprintf(buf, MAX_STR_LEN, fmt, va) + 1 > MAX_STR_LEN)
    xerror("xvsprintf/buffer overflow");
}

void xsprintf(char *buf, char *fmt, ...)
{
  va_list va;
  va_start(va, fmt);
  xvsprintf(buf, fmt, va);
  va_end(va);
}

void xerror(char *fmt, ...)
{
  va_list va;
  char buf[MAX_STR_LEN];
  va_start(va, fmt);
  xvsprintf(buf, fmt, va);
  va_end(va);
  fprintf(stderr, "!%s\n", buf);
  exit(1);
}

void *xmalloc(int size)
{
  void *p;
  if (size == 0) return NULL;
  if ((p = malloc(size)) == NULL) xerror("xmalloc failed");
  return p;
}

void xfree(void *p)
{
  if (p != NULL) free(p);
}

void *xrealloc(void *p, int size)
{
  if (p == NULL) p = xmalloc(size);
  else if (size == 0) {
    xfree(p);
    p = NULL;
  } else {
    if ((p = realloc(p, size)) == NULL) xerror("xrealloc failed");
  }
  return p;
}

char *xstrdup(char *s)
{
  int len;
  char *result;
  len = strlen(s);
  result = xmalloc(len + 1);
  memcpy(result, s, len + 1);
  return result;
}

#ifndef NDEBUG
void xassert_failed(char *fn, int line)
{
  xerror("assert failed at %s:%d", fn, line);
}
#endif
