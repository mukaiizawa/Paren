/*
 *                                 Mulk system.
 *        Copyright (C) 2009-2017 Ken'ichi Tokuoka. All rights reserved.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef INCLUDE_XBARRAY_H
#define INCLUDE_XBARRAY_H

struct xbarray {
  char *elt;
  int size;
  int alloc_size;
};

extern void xbarray_reset(struct xbarray *x);
extern void xbarray_init(struct xbarray *x);
extern void xbarray_free(struct xbarray *x);
extern char *xbarray_reserve(struct xbarray *x,int size);
extern void xbarray_add(struct xbarray *x,int ch);
extern void xbarray_adds(struct xbarray *x,char *s);
extern void xbarray_addf(struct xbarray *x,char *fmt,...);

extern char *xbarray_fgets(struct xbarray *x,FILE *fp);

#endif
