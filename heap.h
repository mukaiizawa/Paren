// heap.

#define HEAP_BLOCK_SIZE 4096

struct heap {
  struct heap_block {
    struct heap_block *next;
    char buf[HEAP_BLOCK_SIZE];
  } *block;
  char *tail;
};

extern struct heap heap_perm;

extern void heap_init(struct heap *h);
extern void *heap_alloc_align(struct heap *h, int size, int align);
extern void *heap_alloc(struct heap *h, int size);
extern void heap_free(struct heap *h);
extern char *heap_strdup(struct heap *h, char *s);
