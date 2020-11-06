// hashmap.

#include "std.h"
#include "hashmap.h"

#define table_index(h, i) ((i) % h->alloc_size)

static void alloc_table(struct hashmap *h)
{
  int i;
  h->size = 0;
  h->table = xmalloc(sizeof(struct hashmap_node) * h->alloc_size);
  for (i = 0; i < h->alloc_size; i++) h->table[i].key = NULL;
}

static void extend(struct hashmap *h)
{
  int i, alloc_size;
  struct hashmap_node *table;
  alloc_size = h->alloc_size;
  table = h->table;
  h->alloc_size += h->alloc_size + 1;
  alloc_table(h);
  for (i = 0; i < alloc_size; i++)
    if (table[i].key != NULL) hashmap_put(h, table[i].key, table[i].val);
  xfree(table);
}

void hashmap_init(struct hashmap *h, int(*hash)(void *p))
{
  h->alloc_size = 0xf;
  h->hash = hash;
  alloc_table(h);
}

void *hashmap_get(struct hashmap *h, void *key)
{
  int i, (*hash)(void *p);
  struct hashmap_node *n;
  hash = h->hash;
  i = hash(key);
  while ((n = &h->table[table_index(h, i)])->key != NULL) {
    if (n->key == key) return n->val;
    i++;
  }
  return NULL;
}

void hashmap_put(struct hashmap *h, void *key, void *val)
{
  int i, (*hash)(void *p);
  struct hashmap_node *n;
  hash = h->hash;
  i = hash(key);
  while ((n = &h->table[table_index(h, i)])->key != NULL) {
    if (n->key == key) {
      n->val = val;
      return;
    }
    i++;
  }
  n->key = key;
  n->val = val;
  if (h->size++ > h->alloc_size * 0.5) extend(h);
}

void hashmap_foreach(struct hashmap *h, void (*f)(void *key, void *val))
{
  int i;
  struct hashmap_node *n;
  for (i = 0; i < h->alloc_size; i++) {
    if ((n = &h->table[i])->key != NULL) (*f)(n->key, n->val);
  }
}

void hashmap_free(struct hashmap *h)
{
  xfree(h->table);
}
