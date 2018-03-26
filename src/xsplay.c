// xc splay tree.

#include "std.h"
#include "xsplay.h"

static struct splay_node splay_null_node;
#define null (&splay_null_node)

static struct splay_node *free_list = NULL;

static struct splay_node *alloc(void)
{
  struct splay_node *n;
  if (free_list == NULL) n = xmalloc(sizeof(struct splay_node));
  else {
    n = free_list;
    free_list = n->left;
  }
  return n;
}

static void release(struct splay_node *n)
{
  xassert(n != null);
  n->left = free_list;
  free_list = n;
}

void splay_init(struct xsplay *s, int (*cmp)(void *p, void *q))
{
  s->top = null;
  s->cmp = cmp;
}

static struct splay_node *balance(struct xsplay *s, void *key)
{
  struct splay_node *top, *p, *q;
  int (*cmp)(void *p, void *q), d;
  top = s->top;
  cmp = s->cmp;
  null->key = key;
  null->left = null->right=null;
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
      } else if (d>0) {
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
    while(p->right != null) p = p->right;
    p->right = r;
  }
  return l;
}

void splay_add(struct xsplay *s, void *k, void *d)
{
  struct splay_node *top, *newtop;
  newtop = alloc();
  top = balance(s, k);
  xassert(top == null);
  newtop->left = null->left;
  newtop->right = null->right;
  newtop->key = k;
  newtop->data = d;
  s->top = newtop;
}

void *splay_find(struct xsplay *s, void *k)
{
  struct splay_node *top;
  top = balance(s, k);
  if (top == null) {
    s->top = resume(top);
    return NULL;
  } else {
    s->top = top;
    return top->data;
  }
}

void splay_delete(struct xsplay *s, void *k)
{
  struct splay_node *top;
  top = balance(s, k);
  xassert(top != null);
  s->top=resume(top);
  release(top);
}

static void free1(struct splay_node *n)
{
  if (n != null) {
    free1(n->left);
    free1(n->right);
    release(n);
  }
}

void splay_free(struct xsplay *s)
{
  free1(s->top);
  s->top = null;
}

static void (*func)(int depth, void *key, void *data) = NULL;

static void foreach1(int depth, struct splay_node *n)
{
  if (n != null) {
    foreach1(depth+1, n->left);
    (*func)(depth, n->key, n->data);
    foreach1(depth+1, n->right);
  }
}

void splay_foreach(struct xsplay *s, void (*f)(int d, void *key, void *data))
{
  xassert(func==NULL);
  func = f;
  foreach1(0, s->top);
  func = NULL;
}
