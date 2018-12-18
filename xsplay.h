// xc splay tree.

struct xsplay {
  struct xsplay_node {
    void *key, *data;
    struct xsplay_node *left, *right;
  } *top;
  int (*cmp)(void *p, void *q);
};

extern void xsplay_init(struct xsplay *s, int (*cmp)(void *p, void *q));
extern void xsplay_add(struct xsplay *s, void *k, void *d);
extern void xsplay_replace(struct xsplay *s, void *k, void *d);
extern void *xsplay_find(struct xsplay *s, void *k);
extern void xsplay_delete(struct xsplay *s, void *k);
extern void xsplay_reset(struct xsplay *s);
extern void xsplay_foreach(struct xsplay *s,
    void (*func)(int depth, void *key, void *data));
