// xc splay tree.

struct xsplay {
  struct splay_node {
    void *key, *data;
    struct splay_node *left, *right;
  } *top;
  int (*cmp)(void *p, void *q);
};

extern void splay_init(struct xsplay *s, int (*cmp)(void *p, void *q));
extern void splay_add(struct xsplay *s, void *k, void *d);
extern void *splay_find(struct xsplay *s, void *k);
extern void splay_delete(struct xsplay *s, void *k);
extern void splay_free(struct xsplay *s);
extern void splay_foreach(struct xsplay *s, 
    void (*func)(int depth, void *key, void *data));
