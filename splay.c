// splay tree.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "splay.h"

int splay_symcmp(object o, object p)
{
  xassert(type_p(o, SYMBOL));
  return (intptr_t)o - (intptr_t)p;
}

int splay_strcmp(object o, object p)
{
  int i, len;
  if ((i = (len = o->barray.size) - p->barray.size) != 0) return i;
  return memcmp(o->barray.elt, p->barray.elt, len);
}

#define nil object_splay_nil
#define K 0
#define V 1
#define L 2
#define R 3

static object balance(object s, object key)
{
  object top, p, q;
  int (*cmp)(object p, object q), d;
  cmp = s->splay.cmp;
  top = s->splay.top;
  nil->array.elt[K] = key;
  nil->array.elt[L] = nil->array.elt[R] = nil;
  while ((d = (*cmp)(key, top->array.elt[K])) != 0) {
    p = top;
    if (d < 0) {
      q = p->array.elt[L];
      if ((d = (*cmp)(key, q->array.elt[K])) == 0) {
        top = q;
        p->array.elt[L] = top->array.elt[R];
        top->array.elt[R] = p;
        break;
      } else if (d < 0) {
        top = q->array.elt[L];
        q->array.elt[L] = top->array.elt[R];
        top->array.elt[R] = p;
      } else {
        top = q->array.elt[R];
        q->array.elt[R] = top->array.elt[L];
        top->array.elt[L] = q;
        p->array.elt[L] = top->array.elt[R];
        top->array.elt[R] = p;
      }
    } else {
      q = p->array.elt[R];
      if ((d = (*cmp)(key, q->array.elt[K])) == 0) {
        top = q;
        p->array.elt[R] = top->array.elt[L];
        top->array.elt[L] = p;
        break;
      } else if (d > 0) {
        top = q->array.elt[R];
        q->array.elt[R] = top->array.elt[L];
        top->array.elt[L] = p;
      } else {
        top = q->array.elt[L];
        q->array.elt[L] = top->array.elt[R];
        top->array.elt[R] = q;
        p->array.elt[R] = top->array.elt[L];
        top->array.elt[L] = p;
      }
    }
  }
  return top;
}

static object resume(object top)
{
  object l, r, p;
  l = top->array.elt[L];
  r = top->array.elt[R];
  if (l == nil) return r;
  if (r != nil) {
    p = l;
    while (p->array.elt[R] != nil) p = p->array.elt[R];
    p->array.elt[R] = r;
  }
  return l;
}

void splay_add(object s, object key, object val)
{
  object top;
  top = balance(s, key);
  xassert(top == nil);
  top = gc_new_splay_node(key, val, nil->array.elt[L], nil->array.elt[R]);
  s->splay.top = top;
}

void splay_replace(object s, object key, object val)
{
  object top;
  top = balance(s, key);
  if (top != nil) top->array.elt[V] = val;
  else top = gc_new_splay_node(key, val, nil->array.elt[L], nil->array.elt[R]);
  s->splay.top = top;
}

object splay_find(object s, object key)
{
  object top;
  top = balance(s, key);
  if (top == nil) {
    s->splay.top = resume(top);
    return NULL;
  }
  s->splay.top = top;
  return top->array.elt[V];
}

static void splay_dump_rec(object node, int level)
{
  int i;
  char buf[MAX_STR_LEN];
  xassert(type_p(node, ARRAY));
  if (node == object_splay_nil) {
    // printf(" nil\n");
    return;
  }
  // for (i = 0; i < level; i++) printf(" ");
  for (i = 0; i < level; i++) printf("	");
  // printf("(%s,", object_describe(node->array.elt[K], buf));
  // printf(" %s)\n", object_describe(node->array.elt[V], buf));
  printf("%s\n", object_describe(node->array.elt[K], buf));
  // printf("L");
  splay_dump_rec(node->array.elt[L], ++level);
  // printf("R");
  splay_dump_rec(node->array.elt[R], level);
}

void splay_dump(object s)
{
  xassert(type_p(s, SPLAY));
  splay_dump_rec(s->splay.top, 0);
}
