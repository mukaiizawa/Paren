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
  if (o == p) return 0;
  os = o->barray.size;
  ps = p->barray.size;
  if ((i = os - ps) == 0) return memcmp(o->barray.elt, p->barray.elt, os);
  if (i > 0) return 1;
  return -1;
}

static object splay_get_top(object splay)
{
  return splay->cons.car;
}

static void splay_set_top(object splay, object top)
{
  splay->cons.car = top;
}

static object node_new(object key, object val)
{
  return gc_new_cons(key, gc_new_cons(val
        , gc_new_cons(object_splay_nil, object_splay_nil)));
}

// #define node_get_key(node) ((node)->cons.car)
static object node_get_key(object node)
{
  return node->cons.car;
}

static void node_set_key(object node, object key)
{
  node->cons.car = key;
}

static object node_get_val(object node)
{
  return node->cons.cdr->cons.car;
}

static void node_set_val(object node, object val)
{
  node->cons.cdr->cons.car = val;
}

static object node_get_left(object node)
{
  return node->cons.cdr->cons.cdr->cons.car;
}

static void node_set_left(object node, object l)
{
  node->cons.cdr->cons.cdr->cons.car = l;
}

static object node_get_right(object node)
{
  return node->cons.cdr->cons.cdr->cons.cdr;
}

static void node_set_right(object node, object r)
{
  node->cons.cdr->cons.cdr->cons.cdr = r;
}

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
  top = node_new(key, val);
  node_set_left(top, node_get_left(object_splay_nil));
  node_set_right(top, node_get_right(object_splay_nil));
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
