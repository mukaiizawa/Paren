/*
	xc splay tree.
	$Id: mulk splay.h 1 2018/11/12 Mon 21:34:18 kt $
*/

struct splay {
  struct splay_node {
    void *key,*data;
    struct splay_node *left,*right;
  } *top;
};

extern void splay_init(struct splay *s);
extern void *splay_find(struct splay *s,void *k);
extern void splay_add(struct splay *s,void *k,void *d);
extern void splay_replace(struct splay *s,void *k,void *d);
extern void splay_free(struct splay *s);
extern void splay_foreach(struct splay *s, void (*func)(void *key, void *data));
