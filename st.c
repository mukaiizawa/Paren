// symbol table.

#include "std.h"
#include "splay.h"
#include "object.h"
#include "mem.h"
#include "st.h"

#define INITIAL_ALLOC_SIZE 255
#define at(s, i) s->table[i % s->alloc_size]

static int hash(char *val, int size)
{
  int i, hval;
  for (i = hval = 0; i < size; i++) hval = hval * 137 + LC(val + i);
  return hval & HASH_MASK;
}

static object *alloc_table(int size)
{
  object *table;
  table = xmalloc(sizeof(object) * size);
  return table;
}

static int eq_p(object sym, char *val, int size)
{
  if (sym == NULL) return FALSE;
  if (sym->bytes.size != size) return FALSE;
  return memcmp(sym->bytes.elt, val, size) == 0;
}

static void extend(struct st *s)
{
  int i;
  object o, *table;
  i = s->alloc_size;
  table = s->table;
  s->table = alloc_table(s->alloc_size += s->alloc_size + 1);
  st_reset(s);
  while (--i >= 0) {
    if ((o = table[i]) == NULL) continue;
    st_put(s, o);
  }
  xfree(table);
}

void st_init(struct st *s)
{
  s->table = alloc_table(s->alloc_size = INITIAL_ALLOC_SIZE);
  st_reset(s);
}

void st_reset(struct st *s)
{
  int i;
  s->size = 0;
  for (i = 0; i < s->alloc_size; i++) s->table[i] = NULL;
}

object st_get(struct st *s, char *val, int size)
{
  int hval;
  object o;
  hval = hash(val, size);
  while ((o = at(s, hval++)) != NULL)
    if (eq_p(o, val, size)) return o;
  return NULL;
}

object st_put(struct st *s, object sym)
{
  int hval;
  hval = hash(sym->bytes.elt, sym->bytes.size);
  while (at(s, hval) != NULL) hval++;
  at(s, hval) = sym;
  if (s->size++ > s->alloc_size / 2) extend(s);
  return sym;
}
