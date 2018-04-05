// extensible array

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

#include "std.h"
#include "xarray.h"

void xarray_reset(struct xarray *x)
{
  x->size=0;
}

void xarray_init(struct xarray *x)
{
  x->elt = NULL;
  x->alloc_size = 0;
  xarray_reset(x);
}

static void extend(struct xarray *x, int size)
{
  int newsize;
  newsize = x->alloc_size;
  if (newsize == 0) newsize = 4;
  while (newsize < size) newsize *= 2;
  x->elt = xrealloc(x->elt, newsize * sizeof(void *));
  x->alloc_size = newsize;
}

void xarray_add(struct xarray *x, void *d)
{
  if (x->size == x->alloc_size) extend(x, x->size + 1);
  x->elt[x->size++] = d;
}

void xarray_free(struct xarray *x)
{
  xfree(x->elt);
}

void xarray_resize(struct xarray *x,int newsize)
{
  while (x->size < newsize) xarray_add(x, NULL);
  x->size = newsize;
}
