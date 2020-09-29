// AA tree.

#include "std.h"
#include "heap.h"
#include "aatree.h"

static struct aatree_node *free_list = NULL;
static struct aatree_node aatree_null_node;
#define null (&aatree_null_node)

static struct aatree_node *alloc(void)
{
  struct aatree_node *n;
  if (free_list == NULL) n = heap_alloc(&heap_perm, sizeof(struct aatree_node));
  else {
    n = free_list;
    free_list = n->left;
  } 
  return n;
}

void aatree_init(struct aatree *a)
{
  a->root = null;
}

static struct aatree_node *skew(struct aatree_node *n)
{
  struct aatree_node *l;
  if (n->left->level == n->level) {
    l = n->left;
    n->left = l->right;
    l->right = n;
    return l;
  }
  return n;
}

static struct aatree_node *split(struct aatree_node *n)
{
  struct aatree_node *r;
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

void *aatree_get(struct aatree *a, void *key)
{
  struct aatree_node *n;
  n = a->root;
  while (n != null) {
    if (key == n->key) return n->val;
    if (cmp(key, n->key) < 0) n = n->left;
    else n = n->right;
  }
  return NULL;
}

static struct aatree_node *put(struct aatree_node *n, void *key, void *val)
{
  struct aatree_node *m;
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
  if (cmp(n->key, key) < 0) n->left = put(n->left, key, val);
  else n->right = put(n->right, key, val);
  return split(skew(n));
}

void aatree_put(struct aatree *a, void *key, void *val)
{
  a->root = put(a->root, key, val);
}

static void free1(struct aatree_node *n)
{
  if (n != null) {
    free1(n->left);
    free1(n->right);
    n->left = free_list;
    free_list = n;
  }
}

void aatree_free(struct aatree *a)
{
  free1(a->root);
  a->root = null;
}

static void foreach1(struct aatree_node *n, void (*f)(void *key, void *val))
{
  if (n == null) return;
  foreach1(n->left, f);
  (*f)(n->key, n->val);
  foreach1(n->right, f);
}

void aatree_foreach(struct aatree *a, void (*f)(void *key, void *val))
{
  foreach1(a->root, f);
}

static void sweep(void *key, void *val)
{
  int *k, *v;
  k = key;
  v = val;
  printf("%d:%d\n", *k, *v);
}

/*
gcc -g -c aatree.c
gcc -o a.exe aatree.o heap.o std.o
*/
int main(int argc, char *argv[])
{
  struct aatree at;
  int a = 1;
  int b = 2;
  int c = 3;
  aatree_init(&at);
  aatree_put(&at, &a, &a);
  aatree_put(&at, &b, &b);
  printf("----\n");
  aatree_foreach(&at, sweep);
  aatree_put(&at, &a, &b);
  printf("----\n");
  aatree_foreach(&at, sweep);
  aatree_put(&at, &c, &b);
  printf("----\n");
  aatree_foreach(&at, sweep);
  return 0;
}
