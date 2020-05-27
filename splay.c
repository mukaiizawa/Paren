// splay tree.

#include "std.h"
#include "heap.h"
#include "splay.h"

static struct splay_node *free_list = NULL;
static struct splay_node splay_null_node;
#define null (&splay_null_node)

static int cmp(void *o, void *p)
{
  return (intptr_t)o - (intptr_t)p;
}

static struct splay_node *alloc(void)
{
  struct splay_node *n;
  if (free_list == NULL) n = heap_alloc(&heap_perm, sizeof(struct splay_node));
  else  {
    n=free_list;
    free_list=n->left;
  } 
  return n;
}

static void release(struct splay_node *n)
{
  xassert(n != null);
  n->left = free_list;
  free_list = n;
}

void splay_init(struct splay *s)
{
  s->top = null;
}

static struct splay_node *balance(struct splay *s, void *key)
{
  int d;
  struct splay_node *top, *p, *q;
  top = s->top;
  null->key = key;
  null->left = null->right = null;
  while ((d = cmp(key, top->key)) != 0) {
    p = top;
    if (d < 0) {
      q = p->left;
      if ((d = cmp(key, q->key)) == 0) {
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
      if ((d = cmp(key, q->key)) == 0) {
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

static struct splay_node *resume(struct splay_node *top)
{
  struct splay_node *l, *r, *p;
  l = top->left;
  r = top->right;
  if (l == null) return r;
  if (r != null) {
    p = l;
    while (p->right != null) p = p->right;
    p->right = r;
  }
  return l;
}

void splay_add(struct splay *s, void *k, void *d)
{
  struct splay_node *top, *newtop;
  newtop = alloc();
  top = balance(s, k);
  newtop->left = null->left;
  newtop->right = null->right;
  newtop->key = k;
  newtop->data = d;
  s->top = newtop;
}

void splay_replace(struct splay *s, void *k, void *d)
{
  struct splay_node *top;
  top = balance(s, k);
  if (top != null) top->data = d;
  else {
    top = alloc();
    top->key = k;
    top->data = d;
    top->left = null->left;
    top->right = null->right;
  }
  s->top = top;
}

void *splay_find(struct splay *s, void *k)
{
  struct splay_node *top;
  top = balance(s, k);
  if (top == null) {
    s->top = resume(top);
    return NULL;
  }
  s->top = top;
  return top->data;
}

static void free1(struct splay_node *n)
{
  if (n != null) {
    free1(n->left);
    free1(n->right);
    release(n);
  }
}

void splay_free(struct splay *s)
{
  free1(s->top);
  s->top = null;
}

static void foreach1(struct splay_node *n, void (*f)(void *key, void *data))
{
  if (n == null) return;
  foreach1(n->left, f);
  (*f)(n->key, n->data);
  foreach1(n->right, f);
}

void splay_foreach(struct splay *s, void (*f)(void *key, void *data))
{
  foreach1(s->top, f);
}
