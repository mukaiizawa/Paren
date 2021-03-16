// symbol table.

#include "std.h"
#include "object.h"
#include "st.h"

#define table_index(s, i) ((i) % s->alloc_size)

static void *alloc_table(int size)
{
  return xmalloc(sizeof(object) * size);
}

static void extend(struct st *s)
{
  int i, alloc_size;
  object o, *table;
  alloc_size = s->alloc_size;
  table = s->table;
  s->table = alloc_table(s->alloc_size += s->alloc_size + 1);
  st_reset(s);
  for (i = 0; i < alloc_size; i++)
    if ((o = table[i]) != NULL) st_put(s, o);
  xfree(table);
}

void st_init(struct st *s)
{
  s->table = alloc_table(s->alloc_size = 0xff);
  st_reset(s);
}

void st_reset(struct st *s)
{
  int i;
  s->size = 0;
  for (i = 0; i < s->alloc_size; i++) s->table[i] = NULL;
}

object st_get(struct st *s, char *val, int size, int hval)
{
  object o;
  while ((o = s->table[table_index(s, hval)]) != NULL) {
    if (o->mem.size == size && memcmp(o->mem.elt, val, size) == 0) return o;
    hval++;
  }
  return NULL;
}

object st_put(struct st *s, object sym)
{
  int i;
  if (s->size++ > s->alloc_size * 0.5) extend(s);
  i = table_index(s, hash(sym));
  while (s->table[i] != NULL) i = table_index(s, i + 1);
  s->table[i] = sym;
  return sym;
}
