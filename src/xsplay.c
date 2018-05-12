// xc splay tree.

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
#include "xsplay.h"

static struct xsplay_node xsplay_null_node;
#define null (&xsplay_null_node)

static struct xsplay_node *free_list = NULL;

static struct xsplay_node *alloc(void)
{
  struct xsplay_node *n;
  if (free_list == NULL) return xmalloc(sizeof(struct xsplay_node));
  else {
    n = free_list;
    free_list = n->left;
  }
  return n;
}

static void release(struct xsplay_node *n)
{
  xassert(n != null);
  n->left = free_list;
  free_list = n;
}

void xsplay_init(struct xsplay *s, int (*cmp)(void *p, void *q))
{
  s->top = null;
  s->cmp = cmp;
}

static struct xsplay_node *balance(struct xsplay *s, void *key)
{
  struct xsplay_node *top, *p, *q;
  int (*cmp)(void *p, void *q), d;
  top = s->top;
  cmp = s->cmp;
  null->key = key;
  null->left = null->right = null;
  while((d = (*cmp)(key, top->key)) != 0) {
    p = top;
    if (d < 0) {
      q = p->left;
      if ((d = (*cmp)(key, q->key)) == 0) {
        top = q;
        p->left = top->right;
        top->right = p;
        break;
      } else if (d < 0) {
        top = q->left;
        q->left = top->right;
        top->right = p;
      } else {
        top = q->right;
        q->right = top->left;
        top->left = q;
        p->left = top->right;
        top->right = p;
      }
    } else {
      q = p->right;
      if ((d = (*cmp)(key, q->key)) == 0) {
        top = q;
        p->right = top->left;
        top->left = p;
        break;
      } else if (d > 0) {
        top = q->right;
        q->right = top->left;
        top->left = p;
      } else {
        top = q->left;
        q->left = top->right;
        top->right = q;
        p->right = top->left;
        top->left = p;
      }
    }
  }
  return top;
}

static struct xsplay_node *resume(struct xsplay_node *top)
{
  struct xsplay_node *l, *r, *p;
  l = top->left;
  r = top->right;
  if (l == null) return r;
  if (r != null) {
    p = l;
    while(p->right != null) p = p->right;
    p->right = r;
  }
  return l;
}

void xsplay_add(struct xsplay *s, void *k, void *d)
{
  struct xsplay_node *top;
  top = balance(s, k);
  xassert(top == null);
  top = alloc();
  top->left = null->left;
  top->right = null->right;
  top->key = k;
  top->data = d;
  s->top = top;
}

void xsplay_replace(struct xsplay *s, void *k, void *d)
{
  if (xsplay_find(s, k) != NULL) xsplay_delete(s, k);
  xsplay_add(s, k, d);
}

void *xsplay_find(struct xsplay *s, void *k)
{
  struct xsplay_node *top;
  top = balance(s, k);
  if (top == null) {
    s->top = resume(top);
    return NULL;
  } else {
    s->top = top;
    return top->data;
  }
}

void xsplay_delete(struct xsplay *s, void *k)
{
  struct xsplay_node *top;
  top = balance(s, k);
  xassert(top != null);
  s->top = resume(top);
  release(top);
}

static void free1(struct xsplay_node *n)
{
  if (n != null) {
    free1(n->left);
    free1(n->right);
    release(n);
  }
}

void xsplay_free(struct xsplay *s)
{
  xsplay_reset(s);
  xfree(s);
}

void xsplay_reset(struct xsplay *s)
{
  free1(s->top);
  s->top = null;
}

static void (*func)(int depth, void *key, void *data) = NULL;

static void foreach1(int depth, struct xsplay_node *n)
{
  if (n != null) {
    foreach1(depth + 1, n->left);
    (*func)(depth, n->key, n->data);
    foreach1(depth + 1, n->right);
  }
}

void xsplay_foreach(struct xsplay *s, void (*f)(int d, void *key, void *data))
{
  xassert(func == NULL);
  func = f;
  foreach1(0, s->top);
  func = NULL;
}
