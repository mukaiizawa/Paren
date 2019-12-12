// splay tree.

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "splay.h"

static int cmp(object o, object p)
{
  int i, os, ps;
  os = o->barray.size;
  ps = p->barray.size;
  if ((i = os - ps) == 0) return memcmp(o->barray.elt, p->barray.elt, os);
  if (i > 0) return 1;
  return -1;
}

#define splay_get_top(splay) ((splay)->cons.car)
#define splay_set_top(splay, top) ((splay)->cons.car = top)
#define node_get_key(node) ((node)->cons.car->cons.car)
#define node_set_key(node, k) ((node)->cons.car->cons.car = k)
#define node_get_val(node) ((node)->cons.car->cons.cdr)
#define node_set_val(node, v) ((node)->cons.car->cons.cdr = v)
#define node_get_left(node) ((node)->cons.cdr->cons.car)
#define node_set_left(node, l) ((node)->cons.cdr->cons.car = l)
#define node_get_right(node) ((node)->cons.cdr->cons.cdr)
#define node_set_right(node, r) ((node)->cons.cdr->cons.cdr = r)

static object balance(object splay, object key)
{
  int d;
  object top, p, q;
  top = splay_get_top(splay);
  node_set_key(object_splay_nil, key);
  node_set_left(object_splay_nil, object_splay_nil);
  node_set_right(object_splay_nil, object_splay_nil);
  while ((d = cmp(key, node_get_key(top))) != 0) {
    p = top;
    if (d < 0) {
      q = node_get_left(p);
      if ((d = cmp(key, node_get_key(q))) == 0) {
        top = q;
        node_set_left(p, node_get_right(top));
        node_set_right(top, p);
        break;
      } else if (d < 0) {
        top = node_get_left(q);
        node_set_left(q, node_get_right(top));
        node_set_right(top, p);
      } else {
        top = node_get_right(q);
        node_set_right(q, node_get_left(top));
        node_set_left(top, q);
        node_set_left(p, node_get_right(top));
        node_set_right(top, p);
      }
    } else {
      q = node_get_right(p);
      if ((d = cmp(key, node_get_key(q))) == 0) {
        top = q;
        node_set_right(p, node_get_left(top));
        node_set_left(top, p);
        break;
      } else if (d > 0) {
        top = node_get_right(q);
        node_set_right(q, node_get_left(top));
        node_set_left(top, p);
      } else {
        top = node_get_left(q);
        node_set_left(q, node_get_right(top));
        node_set_right(top, q);
        node_set_right(p, node_get_left(top));
        node_set_left(top, p);
      }
    }
  }
  return top;
}

static object resume(object top)
{
  object l, r, p;
  l = node_get_left(top);
  r = node_get_right(top);
  if (l == object_splay_nil) return r;
  if (r != object_splay_nil) {
    p = l;
    while (node_get_right(p) != object_splay_nil) p = node_get_right(p);
    node_set_right(p, r);
  }
  return l;
}

void splay_add(object splay, object key, object val)
{
  object top;
  top = balance(splay, key);
  xassert(top == object_splay_nil);
  top = gc_new_splay_node(key, val,
      node_get_left(object_splay_nil), node_get_right(object_splay_nil));
  splay_set_top(splay, top);
}

void splay_replace(object splay, object key, object val)
{
  object o;
  o = splay_find(splay, key);
  if (o == NULL) splay_add(splay, key, val);
  else node_set_val(splay_get_top(splay), val);
}

object splay_find(object splay, object key)
{
  object top;
  top = balance(splay, key);
  if (top == object_splay_nil) {
    splay_set_top(splay, resume(top));
    return NULL;
  }
  splay_set_top(splay, top);
  return node_get_val(top);
}

void splay_delete(object splay, object key)
{
  object top;
  top = balance(splay, key);
  xassert(top != object_splay_nil);
  splay_set_top(splay, resume(top));
}
