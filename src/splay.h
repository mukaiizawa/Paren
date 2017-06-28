/*
  splay tree.
*/

typedef struct Splay {
  int size;
  int (*cmp)(void *key1, void *key2);
  struct SplayNode {
    void *key;
    void *val;
    struct SplayNode *left, *right;
  } *root;
} Splay;

extern void Splay_init(Splay *splay, int (*cmp)(void *key1,void *key2));
extern int Splay_size(Splay *splay);
extern void *Splay_get(Splay *splay, char *key);
extern void Splay_put(Splay *splay, char *key, void *val);
extern void Splay_remove(Splay *splay, char *key);
extern void Splay_free(Splay *splay);
