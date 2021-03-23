// symbol table.

struct st {
  int size;
  int alloc_size;
  object *table;
};

extern void st_init(struct st *s);
extern void st_reset(struct st *s);
extern object st_get(struct st *s, char *val, int size, int hval);
extern object st_put(struct st *s, object sym);
