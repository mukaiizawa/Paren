// paren object.

#include "std.h"
#include "heap.h"
#include "object.h"
#include "xarray.h"
#include "bi.h"
#include "ip.h"

object object_toplevel;
object object_nil;
object object_true;
object object_key;
object object_opt;
object object_rest;
object object_quote;

object object_Class;
object object_class;
object object_symbol;
object object_super;
object object_features;
object object_fields;
object object_Exception;
object object_message;
object object_stack_trace;

int object_hash(object o)
{
  if (sint_p(o)) return sint_val(o) & HASH_MASK;
  return o->header & HASH_MASK;
}

void object_set_hash(object o, int hval)
{
  xassert(object_hash(o) == 0);
  xassert((hval & ~HASH_MASK) == 0);
  o->header |= hval;
}

int object_mem_hash(char *p, int size)
{
  int i, hval;
  for (i = hval = 0; i < size; i++) hval = hval * 137 + LC(p + i);
  return hval & HASH_MASK;
}

int object_number_hash(double val)
{
  int i;
  if (SINT_MIN <= val && val <= SINT_MAX) {
    i = (int)val;
    if (i == val) return i & HASH_MASK;
  }
  return object_mem_hash((char *)&val, sizeof(double));
}

int object_type(object o)
{
  if (sint_p(o)) return SINT;
  return o->header & TYPE_MASK;
}

void object_set_alive(object o)
{
  o->header |= ALIVE_BIT;
}

void object_set_dead(object o)
{
  o->header &= ~ALIVE_BIT;
}

int object_alive_p(object o)
{
  return o->header & ALIVE_BIT;
}

void object_set_type(object o, int type)
{
  xassert((o->header & TYPE_MASK) == 0);
  o->header |= type;
}

void object_reset_type(object o, int type)
{
  o->header &= ~TYPE_MASK;
  object_set_type(o, type);
}

int object_byte_size(object o)
{
  switch (object_type(o)) {
    case XINT:
      return sizeof(struct xint);
    case XFLOAT:
      return sizeof(struct xfloat);
    case CONS:
      return sizeof(struct cons);
    case SYMBOL:
    case STRING:
    case BYTES:
      return sizeof(struct mem) + o->mem.size - 1;
    case ARRAY:
      return sizeof(struct array) + sizeof(object) * (o->array.size - 1);
    case MACRO:
    case FUNC:
      return sizeof(struct proc);
    case ENV:
    case DICT:
      return sizeof(struct map);
    default:
      xassert(FALSE);
      return -1;
  }
}

static void xbarray_add_mem(struct xbarray *x, object o)
{
  memcpy(xbarray_reserve(x, o->mem.size), o->mem.elt, o->mem.size);
}

static void describe_s_expr(object o, struct xbarray *x);
static void describe_cons(object o, struct xbarray *x)
{
  describe_s_expr(o->cons.car, x);
  while ((o = o->cons.cdr) != object_nil) {
    xbarray_add(x, ' ');
    describe_s_expr(o->cons.car, x);
    if (x->size > MAX_STR_LEN) return;
  }
}

static void describe_mem(object o, struct xbarray *x)
{
  int i;
  xbarray_adds(x, "#<");
  for (i = 0; i < o->mem.size; i++) {
    if (i == 5) {
      xbarray_adds(x, "...");
      break;
    }
    xbarray_addf(x, "0x%02x ", LC(o->mem.elt + i));
    if (x->size > MAX_STR_LEN) return;
  }
  xbarray_add(x, '>');
}

static void describe_array(object o, struct xbarray *x)
{
  int i;
  xbarray_adds(x, "#[ ");
  for (i = 0; i < o->array.size; i++) {
    if (i == 5) {
      xbarray_adds(x, "...");
      break;
    }
    describe_s_expr(o->array.elt[i], x);
    xbarray_add(x, ' ');
    if (x->size > MAX_STR_LEN) return;
  }
  xbarray_add(x, ']');
}

static void describe_map(object o, struct xbarray *x)
{
  int i;
  object p;
  xbarray_adds(x, "#{ ");
  for (i = 0; i < o->map.half_size; i++) {
    if ((p = o->map.table[i]) == NULL) continue;
    describe_s_expr(o->map.table[i], x);
    xbarray_add(x, ' ');
    describe_s_expr(o->map.table[i + o->map.half_size], x);
    xbarray_add(x, ' ');
    if (x->size > MAX_STR_LEN) return;
  }
  xbarray_add(x, '}');
}

static void describe_s_expr(object o, struct xbarray *x)
{
  if (x->size > MAX_STR_LEN) return;
  switch (object_type(o)) {
    case SINT:
      xbarray_addf(x, "%d", sint_val(o));
      break;
    case XINT:
      xbarray_addf(x, "%" PRId64, o->xint.val);
      break;
    case XFLOAT:
      xbarray_addf(x, "%g", o->xfloat.val);
      break;
    case CONS:
      xbarray_add(x, '(');
      describe_cons(o, x);
      xbarray_add(x, ')');
      break;
    case STRING:
      xbarray_add(x, '"');
      xbarray_add_mem(x, o);
      xbarray_add(x, '"');
      break;
    case BFUNC:
    case SPECIAL:
      o = o->native.name;
    case SYMBOL:
      xbarray_add_mem(x, o);
      break;
    case BYTES:
      describe_mem(o, x);
      break;
    case ARRAY:
      describe_array(o, x);
      break;
    case DICT:
      describe_map(o, x);
      break;
    case MACRO:
    case FUNC:
      if (object_type(o) == MACRO) xbarray_adds(x, "(macro ");
      else xbarray_adds(x, "(f ");
      if (o->proc.params == object_nil) xbarray_adds(x, "()");
      else describe_s_expr(o->proc.params, x);
      if (o->proc.body != object_nil) {
        xbarray_add(x, ' ');
        describe_cons(o->proc.body, x);
      }
      xbarray_add(x, ')');
      break;
    case ENV:
      xbarray_addf(x, "#(:environment 0x%p :top 0x%p)", o, o->map.top);
      break;
    default:
      xassert(FALSE);
  }
}

char *object_describe(object o, char *buf)
{
  struct xbarray x;
  xassert(o != NULL);
  xbarray_init(&x);
  describe_s_expr(o, &x);
  xbarray_add(&x,'\0');
  if (x.size <= MAX_STR_LEN) memcpy(buf, x.elt, x.size);
  else {
    memcpy(buf, x.elt, MAX_STR_LEN - 4);
    strcpy(buf + MAX_STR_LEN - 4, "...");
  }
  xbarray_free(&x);
  return buf;
}

object object_bool(int b)
{
  if (b) return object_true;
  return object_nil;
}

static int double_eq_p(double x, object p)
{
  int64_t i;
  double d;
  if (bi_may_cint64(p, &i)) return fabs(x - (double)i) < DBL_EPSILON;
  if (bi_may_cdouble(p, &d)) return fabs(x - d) < DBL_EPSILON;
  return FALSE;
}

static int int64_eq_p(int64_t x, object p)
{
  int64_t y;
  if (bi_may_cint64(p, &y)) return x == y;
  return double_eq_p((double)x, p);
}

static int number_eq_p(object o, object p)
{
  int64_t i;
  double d;
  if (bi_may_cint64(o, &i)) return int64_eq_p(i, p);
  if (bi_may_cdouble(o, &d)) return double_eq_p(d, p);
  return FALSE;
}

static int cons_eq_p(object o, object p)
{
  if (object_type(p) != CONS) return FALSE;
  while (TRUE) {
    if (o == object_nil) return p == object_nil;
    if (p == object_nil) return o == object_nil;
    if (!object_eq_p(o->cons.car, p->cons.car)) return FALSE;
    o = o->cons.cdr;
    p = p->cons.cdr;
  }
}

static int bytes_eq_p(object o, object p)
{
  if (object_type(o) != object_type(p)) return FALSE;
  if (o->mem.size != p->mem.size) return FALSE;
  return memcmp(o->mem.elt, p->mem.elt, o->mem.size) == 0;
}

static int array_eq_p(object o, object p)
{
  int i;
  if (object_type(p) != ARRAY) return FALSE;
  if (o->array.size != p->array.size) return FALSE;
  for (i = 0; i < o->array.size; i++)
    if (!object_eq_p(o->array.elt[i], p->array.elt[i])) return FALSE;
  return TRUE;
}

static int dict_eq_p(object o, object p)
{
  object v, keys;
  if (object_type(p) != DICT) return FALSE;
  if (o->map.entry_count != p->map.entry_count) return FALSE;
  keys = map_keys(o);
  while (keys != object_nil) {
    if ((v = map_get(p, keys->cons.car)) == NULL) return FALSE;
    if (!object_eq_p(map_get(o, keys->cons.car), v)) return FALSE;
    keys = keys->cons.cdr;
  }
  return TRUE;
}

int object_eq_p(object o, object p)
{
  if (o == p) return TRUE;
  switch (object_type(o)) {
    case SINT:
    case XINT:
    case XFLOAT:
      return number_eq_p(o, p);
    case BYTES:
    case STRING:
      return bytes_eq_p(o, p);
    case CONS: return cons_eq_p(o, p);
    case ARRAY: return array_eq_p(o, p);
    case DICT: return dict_eq_p(o, p);
    default: return FALSE;
  }
}

int list_len(object o)
{
  int i;
  xassert(list_p(o));
  for (i = 0; object_type(o) == CONS; i++) o = o->cons.cdr;
  return i;
}

object list_reverse(object o)
{
  object p, acc;
  xassert(list_p(o));
  acc = object_nil;
  while (o != object_nil) {
    p = o->cons.cdr;
    o->cons.cdr = acc;
    acc = o;
    o = p;
  }
  return acc;
}

object map_get(object o, object s)
{
  int i;
  object p;
  if (o->map.half_size != 0) {
    i = object_hash(s) % o->map.half_size;
    while ((p = o->map.table[i]) != NULL) {
      if (object_eq_p(p, s)) return o->map.table[i + o->map.half_size];
      if (++i == o->map.half_size) i = 0;
    }
  }
  return NULL;
}

object map_get_propagation(object o, object s)
{
  object p;
  while (o != object_nil) {
    if ((p = map_get(o, s)) != NULL) return p;
    o = o->map.top;
  }
  return NULL;
}

void map_put(object o, object s, object v)
{
  int i;
  object p;
  xassert(o->map.half_size != 0);
  i = object_hash(s) % o->map.half_size;
  while ((p = o->map.table[i]) != NULL) {
    if (object_eq_p(p, s)) {
      o->map.table[i + o->map.half_size] = v;
      return;
    }
    if (++i == o->map.half_size) i = 0;
  }
  o->map.table[i] = s;
  o->map.table[i + o->map.half_size] = v;
  o->map.entry_count++;
  if (o->map.entry_count * 2 > o->map.half_size) gc_extend_table(o);
}

void map_put_propagation(object o, object s, object v)
{
  while (o != object_toplevel) {
    if (map_get(o, s) != NULL) {
      map_put(o, s, v);
      return;
    }
    o = o->map.top;
  }
  map_put(object_toplevel, s, v);
}

void map_foreach(object o, void (*f)(void *s, void *v))
{
  int i;
  object *table;
  table = o->map.table;
  for (i = 0; i < o->map.half_size; i++)
    if (table[i] != NULL) (*f)(table[i], table[i + o->map.half_size]);
}

object map_keys(object o)
{
  int i;
  object keys, *table;
  xassert(object_type(o) == DICT);
  table = o->map.table;
  keys = object_nil;
  for (i = 0; i < o->map.half_size; i++)
    if (table[i] != NULL) keys = gc_new_cons(table[i], keys);
  return keys;
}

static int gc_used_memory;

static struct heap heap;
static struct symbol_table st;

#define LINK0_SIZE (sizeof(struct cons))
#define LINK1_SIZE (sizeof(struct map))
static object link0, link1;

#define regist(o) (xarray_add(table, o))
static struct xarray *table, *table_wk, table0, table1;

static void *gc_alloc(int size);
static void gc_free0(int size, void *p);

// symbol table

#define symbol_table_index(st, i) ((i) % st->alloc_size)
#define symbol_table_bytelen(st) (gc_alloc(sizeof(object) * (st->size)))
#define symbol_table_alloc(size) (gc_alloc(sizeof(object) * (size)))

struct symbol_table {
  int size;
  int alloc_size;
  object *table;
};

static void symbol_table_reset(struct symbol_table *st)
{
  int i;
  st->size = 0;
  for (i = 0; i < st->alloc_size; i++) st->table[i] = NULL;
}

static void symbol_table_init(struct symbol_table *st)
{
  st->alloc_size = 0x1000;    // rule of thumb.
  st->table = gc_alloc(sizeof(object) * st->alloc_size);
  symbol_table_reset(st);
}

static object symbol_table_get(struct symbol_table *st, char *val, int size, int hval)
{
  object o;
  while ((o = st->table[symbol_table_index(st, hval)]) != NULL) {
    if (o->mem.size == size && memcmp(o->mem.elt, val, size) == 0) return o;
    hval++;
  }
  return NULL;
}

static object symbol_table_put(struct symbol_table *st, object sym)
{
  int i, alloc_size;
  object o, *table;
  if (st->size++ > (alloc_size = st->alloc_size) * 0.5) {
    table = st->table;
    st->alloc_size *= 2;
    st->table = gc_alloc(sizeof(object) * st->alloc_size);
    symbol_table_reset(st);
    for (i = 0; i < alloc_size; i++)
      if ((o = table[i]) != NULL) symbol_table_put(st, o);
    gc_free0(alloc_size, table);
  }
  i = symbol_table_index(st, object_hash(sym));
  while (st->table[i] != NULL) i = symbol_table_index(st, i + 1);
  st->table[i] = sym;
  return sym;
}

// memory allocater & garbage collector

static void *gc_alloc(int size)
{
  object o;
  if (size <= LINK0_SIZE) {
    size = LINK0_SIZE;
    if (link0 == NULL) o = heap_alloc(&heap, size);
    else {
      o = link0;
      link0 = o->next;
    }
  } else if (size <= LINK1_SIZE) {
    size = LINK1_SIZE;
    if (link1 == NULL) o = heap_alloc(&heap, size);
    else {
      o = link1;
      link1 = o->next;
    }
  } else o = xmalloc(size);
  gc_used_memory += size;
  o->header &= 0;
  return o;
}

static void gc_free0(int size, void *p)
{
  object o;
  o = p;
  if (size <= LINK0_SIZE) {
    size = LINK0_SIZE;
    o->next = link0;
    link0 = o;
  } else if (size <= LINK1_SIZE) {
    size = LINK1_SIZE;
    o->next = link1;
    link1 = o;
  } else xfree(o);
  gc_used_memory -= size;
}

void gc_free(object o)
{
  switch (object_type(o)) {
    case DICT:
    case ENV:
      if (o->map.half_size != 0) gc_free0(sizeof(object) * o->map.half_size * 2, o->map.table);
      break;
    default:
      break;
  }
  gc_free0(object_byte_size(o), o);
}

object gc_new_xint(int64_t val)
{
  object o;
  if (SINT_MIN <= val && val <= SINT_MAX) return sint((int)val);
  o = gc_alloc(sizeof(struct xint));
  object_set_type(o, XINT);
  object_set_hash(o, object_number_hash((double)val));
  o->xint.val = val;
  regist(o);
  return o;
}

object gc_new_xfloat(double val)
{
  object o;
  o = gc_alloc(sizeof(struct xfloat));
  object_set_type(o, XFLOAT);
  object_set_hash(o, object_number_hash(val));
  o->xfloat.val = val;
  regist(o);
  return o;
}

static object new_cons(void)
{
  object o;
  o = gc_alloc(sizeof(struct cons));
  object_set_type(o, CONS);
  regist(o);
  return o;
}

object gc_new_cons(object car, object cdr)
{
  object o;
  o = new_cons();
  o->cons.car = car;
  o->cons.cdr = cdr;
  return o;
}

object gc_copy_cons(object o, object *tail)
{
  object head;
  if (o == object_nil) return object_nil;
  head = *tail = new_cons();
  (*tail)->cons.car = o->cons.car;
  while ((o = o->cons.cdr) != object_nil) {
    *tail = (*tail)->cons.cdr = new_cons();
    (*tail)->cons.car = o->cons.car;
  }
  (*tail)->cons.cdr = object_nil;
  return head;
}

static object new_mem(int type, int size)
{
  object o;
  xassert(size >= 0);
  o = gc_alloc(sizeof(struct mem) + size - 1);
  object_set_type(o, type);
  o->mem.size = size;
  regist(o);
  return o;
}

object gc_new_mem(int type, int size)
{
  object o;
  o = new_mem(type, size);
  memset(o->mem.elt, 0, size);
  return o;
}

static object new_mem_from(int type, char *val, int size)
{
  object o;
  o = new_mem(type, size);
  memcpy(o->mem.elt, val, size);
  return o;
}

object gc_new_mem_from(int type, char *val, int size)
{
  int hval;
  object o;
  switch (type) {
    case SYMBOL:
      hval = object_mem_hash(val, size);
      if ((o = symbol_table_get(&st, val, size, hval)) != NULL) return o;
      o = new_mem_from(type, val, size);
      object_set_hash(o, hval);
      return symbol_table_put(&st, o);
    case STRING:
      o = new_mem_from(type, val, size);
      object_set_hash(o, object_mem_hash(val, size));
      return o;
    case BYTES:
      o = new_mem_from(type, val, size);
      return o;
    default:
      xassert(FALSE);
      return NULL;
  }
}

object gc_new_mem_from_cstr(int type, char *cstr)
{
  return gc_new_mem_from(type, cstr, strlen(cstr));
}

object gc_new_mem_from_xbarray(int type, struct xbarray *x)
{
  return gc_new_mem_from(type, x->elt, x->size);
}

object gc_new_cstring(object o)
{
  object p;
  p = gc_new_mem_from(STRING, o->mem.elt, o->mem.size + 1);
  SC(p->mem.elt + o->mem.size, '\0');
  return p;
}

static object new_array(int size)
{
  object o;
  xassert(size >= 0);
  o = gc_alloc(sizeof(struct array) + sizeof(object) * (size - 1));
  object_set_type(o, ARRAY);
  o->array.size = size;
  regist(o);
  return o;
}

object gc_new_array(int size)
{
  int i;
  object o;
  o = new_array(size);
  for (i = 0; i < size; i++) o->array.elt[i] = object_nil;
  return o;
}

object gc_new_array_from(object *o, int size)
{
  object p;
  p = new_array(size);
  memcpy(p->array.elt, o, size * sizeof(object));
  return p;
}

static object new_map(int type, int half_size, object top)
{
  int i;
  object o;
  o = gc_alloc(sizeof(struct map));
  object_set_type(o, type);
  o->map.top = top;
  o->map.entry_count = 0;
  o->map.half_size = half_size;
  if (half_size != 0) o->map.table = gc_alloc(sizeof(object) * half_size * 2);
  for (i = 0; i < half_size; i++) o->map.table[i] = NULL;
  regist(o);
  return o;
}

object gc_new_dict(void)
{
  return new_map(DICT, 8, object_nil);
}

static object new_proc(int type, object env, int param_count, object params, object body)
{
  object o;
  xassert(object_type(env) == ENV);
  o = gc_alloc(sizeof(struct proc));
  o->proc.env = env;
  o->proc.param_count = param_count;
  o->proc.params = params;
  o->proc.body = body;
  object_set_type(o, type);
  regist(o);
  return o;
}

object gc_new_macro(object env, int param_count, object params, object body)
{
  return new_proc(MACRO, env, param_count, params, body);
}

object gc_new_func(object env, int param_count, object params, object body)
{
  return new_proc(FUNC, env, param_count, params, body);
}

object gc_new_native(int type, object name, void *p)
{
  object o;
  o = gc_alloc(sizeof(struct native));
  o->native.name = name;
  o->native.u.p = p;
  object_set_type(o, type);
  regist(o);
  return o;
}

object gc_new_env(object top, int half_size)
{
  return new_map(ENV, half_size, top);
}

void gc_extend_table(object o)
{
  int i, half_size;
  object *table;
  table = o->map.table;
  half_size = o->map.half_size;
  o->map.entry_count = 0;
  o->map.half_size *= 2;
  o->map.table = gc_alloc(sizeof(object) * o->map.half_size * 2);
  for (i = 0; i < o->map.half_size; i++) o->map.table[i] = NULL;
  for (i = 0; i < half_size; i++)
    if (table[i] != NULL) map_put(o, table[i], table[i + half_size]);
  gc_free0(sizeof(object) * half_size * 2, table);
}

static void mark_binding(void *key, void *val)
{
  gc_mark(key);
  gc_mark(val);
}

void gc_mark(object o)
{
  int i;
  if (sint_p(o)) return;
  if (object_alive_p(o)) return;
  object_set_alive(o);
  switch (object_type(o)) {
    case CONS:
      while (o != object_nil) {
        gc_mark(o->cons.car);
        o = o->cons.cdr;
        object_set_alive(o);    // for stack overflow
      }
      break;
    case ARRAY:
      for (i = 0; i < o->array.size; i++) gc_mark(o->array.elt[i]);
      break;
    case DICT:
      map_foreach(o, mark_binding);
      break;
    case SPECIAL:
    case BFUNC:
      gc_mark(o->native.name);
      break;
    case MACRO:
    case FUNC:
      gc_mark(o->proc.env);
      gc_mark(o->proc.params);
      gc_mark(o->proc.body);
      break;
    case ENV:
      gc_mark(o->map.top);
      map_foreach(o, mark_binding);
    default:
      break;
  }
}

static void switch_table(void)
{
  struct xarray *p;
  p = table_wk;
  table_wk = table;
  table = p;
}

static void sweep_s_expr(void)
{
  int i;
  object o;
  switch_table();
  xarray_reset(table);
  symbol_table_reset(&st);
  for (i = 0; i < table_wk->size; i++) {
    o = table_wk->elt[i];
    if (!object_alive_p(o)) gc_free(o);
    else {
      object_set_dead(o);
      switch (object_type(o)) {
        case SYMBOL: symbol_table_put(&st, o); break;
        default: break;
      }
      regist(o);
    }
  }
}

void gc_chance(void)
{
  if (gc_used_memory < GC_CHANCE_MEMORY) return;
  if (GC_LOG_P) fprintf(stderr, "before gc(used memory %d[byte])\n", gc_used_memory);
  ip_mark_object();
  sweep_s_expr();
  if (GC_LOG_P) fprintf(stderr, "after gc(used memory %d[byte])\n", gc_used_memory);
}

void gc_init(void)
{
  gc_used_memory = 0;
  link0 = link1 = NULL;
  symbol_table_init(&st);
  heap_init(&heap);
  xarray_init(&table0);
  xarray_init(&table1);
  table = &table0;
  table_wk = &table1;
}
