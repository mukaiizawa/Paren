// xc extensible byte array.

/* lisence {{{
								  Mulk system.
		 Copyright (C) 2009-2018 Ken'ichi Tokuoka. All rights reserved.

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

#include <string.h>

#include "std.h"
#include "xbarray.h"

void xbarray_reset(struct xbarray *x)
{
  x->size = 0;
}

void xbarray_init(struct xbarray *x)
{
  x->elt = NULL;
  x->alloc_size = 0;
  xbarray_reset(x);
}

void xbarray_free(struct xbarray *x)
{
  xfree(x->elt);
}

static void extend(struct xbarray *x, int size)
{
  int newsize;
  newsize = x->alloc_size;
  if (newsize == 0) newsize = 16;
  while (newsize < size) newsize *= 2;
  xassert(newsize != x->alloc_size);
  x->elt = xrealloc(x->elt, newsize);
  x->alloc_size = newsize;
}

char *xbarray_reserve(struct xbarray *x, int size)
{
  char *p;
  if(x->alloc_size<x->size+size) extend(x, x->size+size);
  p = x->elt+x->size;
  x->size += size;
  return p;
}

void xbarray_add(struct xbarray *x, int ch)
{
  *xbarray_reserve(x, 1) = ch;
}

void xbarray_adds(struct xbarray *x, char *s)
{
  int len;
  len = strlen(s);
  memcpy(xbarray_reserve(x, len), s, len);
}

void xbarray_addf(struct xbarray *x, char *fmt, ...)
{
  char buf[MAX_STR_LEN];
  va_list va;
  va_start(va, fmt);
  xvsprintf(buf, fmt, va);
  va_end(va);
  xbarray_adds(x, buf);
}

char *xbarray_fgets(struct xbarray *x, FILE *fp)
{
  int ch;
  xbarray_reset(x);
  while(TRUE) {
    ch = fgetc(fp);
    if(ch == EOF) return NULL;
    if(ch == '\n') break;
    if(ch == '\r') {
      ch = fgetc(fp);
      if(ch != '\n') ungetc(ch, fp);
      break;
    }
    xbarray_add(x, ch);
  }
  xbarray_add(x, '\0');
  return x->elt;
}
