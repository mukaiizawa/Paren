// AA tree.

struct aatree {
  struct aatree_node {
    int level;
    void *key, *val;
    struct aatree_node *left, *right;
  } *root;
};

extern void aatree_init(struct aatree *a);
extern void *aatree_get(struct aatree *a, void *key);
extern void aatree_put(struct aatree *a, void *key, void *val);
extern void aatree_free(struct aatree *a);
extern void aatree_foreach(struct aatree *a, void (*f)(void *key, void *val));
