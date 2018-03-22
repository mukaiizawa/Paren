// xc extensible byte array.

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
