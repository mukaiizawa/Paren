// heap.

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

#include <string.h>
#include <stddef.h>

#include "heap.h"

struct heap heap_perm = {NULL, NULL};

void heap_init(struct heap *h)
{
  h->block=NULL;
  h->tail=NULL;
}

static int gap_to_align(int offset, int align)
{
  if (offset % align == 0) return 0;
  return align - offset % align;
}

#define BLOCK_TOP offsetof(struct heap_block, buf[0])

static void *independent_alloc(struct heap *h, int size, int align)
{
  int gap;
  struct heap_block *n;
  gap = gap_to_align(BLOCK_TOP, align);
  n = xmalloc(BLOCK_TOP + gap + size);
  if (h->block == NULL) h->block = n;
  else {
    n->next = h->block->next;
    h->block->next = n;
  }
  return n->buf + gap;
}

void *heap_alloc_align(struct heap *h, int size, int align)
{
  int topgap, used,gap;
  struct heap_block *n;
  if (size == 0) return NULL;
  topgap = gap_to_align(BLOCK_TOP, align);
  if (topgap+size > HEAP_BLOCK_SIZE) return independent_alloc(h, size, align);
  if (h->tail != NULL) {
    used = (int)(h->tail - h->block->buf);
    gap = gap_to_align(BLOCK_TOP + used, align);
    if (used + gap + size <= HEAP_BLOCK_SIZE) {
      h->tail += gap + size;
      return h->tail-size;
    } else if (used < HEAP_BLOCK_SIZE / 2) {
      return independent_alloc(h, size, align);
    }
  }
  n = xmalloc(sizeof(struct heap_block));
  n->next = h->block;
  h->block = n;
  h->tail = n->buf + topgap + size;
  return h->tail-size;	
}

void *heap_alloc(struct heap *h, int size)
{
  return heap_alloc_align(h, size, sizeof(void *));
}

void heap_free(struct heap *h)
{
  struct heap_block *b, *n;
  b = h->block;
  while (b != NULL) {
    n = b->next;
    xfree(b);
    b = n;
  }
}

char *heap_strdup(struct heap *h, char *s)
{
  return strcpy(heap_alloc_align(h, strlen(s) + 1, 1), s);
}
