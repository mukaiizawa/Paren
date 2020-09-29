// AA tree.

#include "std.h"
#include "heap.h"
#include "at.h"

static struct at_node *free_list = NULL;
static struct at_node at_null_node;
#define null (&at_null_node)

static struct at_node *alloc(void)
{
  struct at_node *n;
  if (free_list == NULL) n = heap_alloc(&heap_perm, sizeof(struct at_node));
  else {
    n = free_list;
    free_list = n->left;
  } 
  return n;
}

void at_init(struct at *a)
{
  a->root = null;
}

static struct at_node *skew(struct at_node *n)
{
  struct at_node *l;
  if (n->left->level == n->level) {
    l = n->left;
    n->left = l->right;
    l->right = n;
    return l;
  }
  return n;
}

static struct at_node *split(struct at_node *n)
{
  struct at_node *r;
  if (n->level == n->right->right->level) {
    r = n->right;
    n->right = r->left;
    r->left = n;
    r->level = r->level + 1;
    return r;
  }
  return n;
}

#define cmp(o, p) ((intptr_t)(o) - (intptr_t)(p))

void *at_get(struct at *a, void *key)
{
  struct at_node *n;
  n = a->root;
  while (n != null) {
    if (key == n->key) return n->val;
    if (cmp(key, n->key) < 0) n = n->left;
    else n = n->right;
  }
  return NULL;
}

static struct at_node *put(struct at_node *n, void *key, void *val)
{
  struct at_node *m;
  if (n == null) {
    m = alloc();
    m->level = 1;
    m->key = key;
    m->val = val;
    m->left = m->right = null;
    return m;
  }
  if (n->key == key) {
    n->val = val;
    return n;
  }
  if (cmp(key, n->key) < 0) n->left = put(n->left, key, val);
  else n->right = put(n->right, key, val);
  return split(skew(n));
}

void at_put(struct at *a, void *key, void *val)
{
  a->root = put(a->root, key, val);
}

static void free1(struct at_node *n)
{
  if (n != null) {
    free1(n->left);
    free1(n->right);
    n->left = free_list;
    free_list = n;
  }
}

void at_free(struct at *a)
{
  free1(a->root);
  a->root = null;
}

static void foreach1(struct at_node *n, void (*f)(void *key, void *val))
{
  if (n == null) return;
  foreach1(n->left, f);
  (*f)(n->key, n->val);
  foreach1(n->right, f);
}

void at_foreach(struct at *a, void (*f)(void *key, void *val))
{
  foreach1(a->root, f);
}
