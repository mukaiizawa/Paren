// hashmap.

struct hashmap {
  int size;
  int alloc_size;
  struct hashmap_node {
    void *key, *val;
  } *table;
  int (*hash)(void *p);
};

extern void hashmap_init(struct hashmap *h, int (*hash)(void *p));
extern void *hashmap_get(struct hashmap *h, void *key);
extern void hashmap_put(struct hashmap *h, void *key, void *val);
extern void hashmap_foreach(struct hashmap *h, void (*f)(void *key, void *val));
extern void hashmap_free(struct hashmap *h);
