// AA tree.

struct at {
  struct at_node {
    int level;
    void *key, *val;
    struct at_node *left, *right;
  } *root;
};

extern void at_init(struct at *a);
extern void *at_get(struct at *a, void *key);
extern void at_put(struct at *a, void *key, void *val);
extern void at_free(struct at *a);
extern void at_foreach(struct at *a, void (*f)(void *key, void *val));
