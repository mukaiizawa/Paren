// paren object.

#include "std.h"
#include "heap.h"
#include "om.h"
#include "xarray.h"
#include "bi.h"
#include "ip.h"

static int used_memory;

static struct heap heap;
static struct symbol_table st;

#define LINK0_SIZE (sizeof(struct cons))
#define LINK1_SIZE (sizeof(struct map))
static object heap_link0, heap_link1;

#define regist(o) (xarray_add(table, o))
static struct xarray *table, *table_wk, table0, table1;

static void *om_alloc(int size);
static void om_free0(int size, void *p);

object om_toplevel;
object om_nil;
object om_true;
object om_key;
object om_opt;
object om_rest;
object om_quote;
object om_Class;
object om_class;
object om_symbol;
object om_super;
object om_features;
object om_fields;
object om_Exception;
object om_message;
object om_stack_trace;

int om_hash(object o)
{
  if (om_sint_p(o)) return om_sint_val(o) & HASH_MASK;
  return o->header & HASH_MASK;
}

static int mem_hash(char *p, int size)
{
  int i, hval;
  for (i = hval = 0; i < size; i++) hval = hval * 137 + LC(p + i);
  return hval & HASH_MASK;
}

static int num_hash(double val)
{
  int i;
  if (SINT_MIN <= val && val <= SINT_MAX) {
    i = (int)val;
    if (i == val) return i & HASH_MASK;
  }
  return mem_hash((char *)&val, sizeof(double));
}

static void set_hash(object o, int hval)
{
  xassert(om_hash(o) == 0);
  xassert((hval & ~HASH_MASK) == 0);
  o->header |= hval;
}

int om_type(object o)
{
  if (om_sint_p(o)) return SINT;
  return o->header & TYPE_MASK;
}

static void set_alive(object o)
{
  o->header |= ALIVE_BIT;
}

static void set_dead(object o)
{
  o->header &= ~ALIVE_BIT;
}

static int alive_p(object o)
{
  return o->header & ALIVE_BIT;
}

static void set_type(object o, int type)
{
  xassert((o->header & TYPE_MASK) == 0);
  o->header |= type;
}

static void xbarray_add_mem(struct xbarray *x, object o)
{
  memcpy(xbarray_reserve(x, o->mem.size), o->mem.elt, o->mem.size);
}

static void describe_s_expr(object o, struct xbarray *x);
static void describe_cons(object o, struct xbarray *x)
{
  describe_s_expr(o->cons.car, x);
  while ((o = o->cons.cdr) != om_nil) {
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
  switch (om_type(o)) {
    case SINT:
      xbarray_addf(x, "%d", om_sint_val(o));
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
      if (om_type(o) == MACRO) xbarray_adds(x, "(macro ");
      else xbarray_adds(x, "(f ");
      if (o->proc.params == om_nil) xbarray_adds(x, "()");
      else describe_s_expr(o->proc.params, x);
      if (o->proc.body != om_nil) {
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

char *om_describe(object o, char *buf)
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

// symbol table

#define symbol_table_index(st, i) ((i) % st->alloc_size)
#define symbol_table_bytelen(st) (om_alloc(sizeof(object) * (st->size)))
#define symbol_table_alloc(size) (om_alloc(sizeof(object) * (size)))

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
  st->table = om_alloc(sizeof(object) * st->alloc_size);
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
    st->table = om_alloc(sizeof(object) * st->alloc_size);
    symbol_table_reset(st);
    for (i = 0; i < alloc_size; i++)
      if ((o = table[i]) != NULL) symbol_table_put(st, o);
    om_free0(alloc_size, table);
  }
  i = symbol_table_index(st, om_hash(sym));
  while (st->table[i] != NULL) i = symbol_table_index(st, i + 1);
  st->table[i] = sym;
  return sym;
}

// constructor & garbage collector

static void *om_alloc(int size)
{
  object o;
  if (size <= LINK0_SIZE) {
    size = LINK0_SIZE;
    if (heap_link0 == NULL) o = heap_alloc(&heap, size);
    else {
      o = heap_link0;
      heap_link0 = o->next;
    }
  } else if (size <= LINK1_SIZE) {
    size = LINK1_SIZE;
    if (heap_link1 == NULL) o = heap_alloc(&heap, size);
    else {
      o = heap_link1;
      heap_link1 = o->next;
    }
  } else o = xmalloc(size);
  used_memory += size;
  o->header &= 0;
  return o;
}

static void om_free0(int size, void *p)
{
  object o;
  o = p;
  if (size <= LINK0_SIZE) {
    size = LINK0_SIZE;
    o->next = heap_link0;
    heap_link0 = o;
  } else if (size <= LINK1_SIZE) {
    size = LINK1_SIZE;
    o->next = heap_link1;
    heap_link1 = o;
  } else xfree(o);
  used_memory -= size;
}

static int om_byte_size(object o)
{
  switch (om_type(o)) {
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

void om_free(object o)
{
  switch (om_type(o)) {
    case DICT:
    case ENV:
      if (o->map.half_size != 0) om_free0(sizeof(object) * o->map.half_size * 2, o->map.table);
      break;
    default:
      break;
  }
  om_free0(om_byte_size(o), o);
}

object om_new_xint(int64_t val)
{
  object o;
  if (SINT_MIN <= val && val <= SINT_MAX) return om_sint((int)val);
  o = om_alloc(sizeof(struct xint));
  set_type(o, XINT);
  set_hash(o, num_hash((double)val));
  o->xint.val = val;
  regist(o);
  return o;
}

object om_new_xfloat(double val)
{
  object o;
  o = om_alloc(sizeof(struct xfloat));
  set_type(o, XFLOAT);
  set_hash(o, num_hash(val));
  o->xfloat.val = val;
  regist(o);
  return o;
}

static object new_cons(void)
{
  object o;
  o = om_alloc(sizeof(struct cons));
  set_type(o, CONS);
  regist(o);
  return o;
}

object om_new_cons(object car, object cdr)
{
  object o;
  o = new_cons();
  o->cons.car = car;
  o->cons.cdr = cdr;
  return o;
}

object om_copy_cons(object o, int size)
{
  xassert(om_type(o) == CONS);
  object head = new_cons();
  object tail = head;
  tail->cons.car = o->cons.car;
  while ((o = o->cons.cdr) != om_nil) {
    if (size && --size == 0) break;    // if size == 0, entire cons.
    tail = tail->cons.cdr = new_cons();
    tail->cons.car = o->cons.car;
  }
  tail->cons.cdr = om_nil;
  return head;
}

object om_coerce_mem_string(object o)
{
  xassert(om_type(o) == BYTES);
  o->header &= ~TYPE_MASK;
  set_type(o, STRING);
  set_hash(o, mem_hash(o->mem.elt, o->mem.size));
  return o;
}

static object new_mem(int type, int size)
{
  xassert(size >= 0);
  object o = om_alloc(sizeof(struct mem) + size - 1);
  set_type(o, type);
  o->mem.size = size;
  regist(o);
  return o;
}

object om_new_mem(int type, int size)
{
  object o = new_mem(type, size);
  memset(o->mem.elt, 0, size);
  return o;
}

static object new_mem_from(int type, char *val, int size)
{
  object o = new_mem(type, size);
  memcpy(o->mem.elt, val, size);
  return o;
}

object om_new_mem_from(int type, char *val, int size)
{
  object o;
  switch (type) {
    case SYMBOL:
      int hval = mem_hash(val, size);
      if ((o = symbol_table_get(&st, val, size, hval)) != NULL) return o;
      o = new_mem_from(type, val, size);
      set_hash(o, hval);
      return symbol_table_put(&st, o);
    case STRING:
      o = new_mem_from(type, val, size);
      set_hash(o, mem_hash(val, size));
      return o;
    case BYTES:
      return new_mem_from(type, val, size);
    default:
      xassert(FALSE);
      return NULL;
  }
}

object om_new_mem_from_cstr(int type, char *cstr)
{
  return om_new_mem_from(type, cstr, strlen(cstr));
}

object om_new_mem_from_xbarray(int type, struct xbarray *x)
{
  return om_new_mem_from(type, x->elt, x->size);
}

object om_new_cstring(object o)
{
  object p = om_new_mem_from(STRING, o->mem.elt, o->mem.size + 1);
  SC(p->mem.elt + o->mem.size, '\0');
  return p;
}

static object new_array(int size)
{
  object o;
  xassert(size >= 0);
  o = om_alloc(sizeof(struct array) + sizeof(object) * (size - 1));
  set_type(o, ARRAY);
  o->array.size = size;
  regist(o);
  return o;
}

object om_new_array(int size)
{
  object o = new_array(size);
  for (int i = 0; i < size; i++) o->array.elt[i] = om_nil;
  return o;
}

object om_new_array_from(object *o, int size)
{
  object p = new_array(size);
  memcpy(p->array.elt, o, size * sizeof(object));
  return p;
}

static object new_map(int type, int half_size, object top)
{
  object o = om_alloc(sizeof(struct map));
  set_type(o, type);
  o->map.top = top;
  o->map.entry_count = 0;
  o->map.half_size = half_size;
  if (half_size != 0) o->map.table = om_alloc(sizeof(object) * half_size * 2);
  for (int i = 0; i < half_size; i++) o->map.table[i] = NULL;
  regist(o);
  return o;
}

object om_new_dict(void)
{
  return new_map(DICT, 8, om_nil);
}

static object new_proc(int type, object env, int param_count, object params, object body)
{
  xassert(om_type(env) == ENV);
  object o = om_alloc(sizeof(struct proc));
  o->proc.env = env;
  o->proc.param_count = param_count;
  o->proc.params = params;
  o->proc.body = body;
  set_type(o, type);
  regist(o);
  return o;
}

object om_new_macro(object env, int param_count, object params, object body)
{
  return new_proc(MACRO, env, param_count, params, body);
}

object om_new_func(object env, int param_count, object params, object body)
{
  return new_proc(FUNC, env, param_count, params, body);
}

object om_new_native(int type, object name, void *p)
{
  object o = om_alloc(sizeof(struct native));
  o->native.name = name;
  o->native.u.p = p;
  set_type(o, type);
  regist(o);
  return o;
}

object om_new_env(object top, int half_size)
{
  return new_map(ENV, half_size, top);
}

static void mark_binding(void *key, void *val)
{
  om_mark(key);
  om_mark(val);
}

void om_mark(object o)
{
  if (om_sint_p(o) || alive_p(o)) return;
  set_alive(o);
  switch (om_type(o)) {
    case CONS:
      while (o != om_nil) {
        om_mark(o->cons.car);
        o = o->cons.cdr;
        set_alive(o);    // for stack overflow
      }
      break;
    case ARRAY:
      for (int i = 0; i < o->array.size; i++)
        om_mark(o->array.elt[i]);
      break;
    case DICT:
      om_map_foreach(o, mark_binding);
      break;
    case SPECIAL:
    case BFUNC:
      om_mark(o->native.name);
      break;
    case MACRO:
    case FUNC:
      om_mark(o->proc.env);
      om_mark(o->proc.params);
      om_mark(o->proc.body);
      break;
    case ENV:
      om_mark(o->map.top);
      om_map_foreach(o, mark_binding);
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

static void mark_and_sweep(void)
{
  ip_mark_object();
  switch_table();
  xarray_reset(table);
  symbol_table_reset(&st);
  for (int i = 0; i < table_wk->size; i++) {
    object o = table_wk->elt[i];
    if (!alive_p(o)) om_free(o);
    else {
      set_dead(o);
      if (om_type(o) == SYMBOL) symbol_table_put(&st, o);
      regist(o);
    }
  }
}

void om_gc_chance(void)
{
  if (used_memory < om_gc_chance_MEMORY) return;
  if (GC_LOG_P) fprintf(stderr, "before gc(used memory %d[byte])\n", used_memory);
  mark_and_sweep();
  if (GC_LOG_P) fprintf(stderr, "after gc(used memory %d[byte])\n", used_memory);
}

// object manipulation.

object om_bool(int b)
{
  if (b) return om_true;
  return om_nil;
}

static int double_eq_p(double x, object p)
{
  double y;
  if (!bi_may_cdouble(p, &y)) return FALSE;
  return fabs(x - y) < DBL_EPSILON;
}

static int int64_eq_p(int64_t x, object p)
{
  int64_t y;
  if (!bi_may_cint64(p, &y)) return double_eq_p((double)x, p);
  return x == y;
}

static int cons_eq_p(object o, object p)
{
  if (om_type(p) != CONS) return FALSE;
  while (TRUE) {
    if (o == om_nil) return p == om_nil;
    if (p == om_nil) return o == om_nil;
    if (!om_eq_p(o->cons.car, p->cons.car)) return FALSE;
    o = o->cons.cdr;
    p = p->cons.cdr;
  }
}

static int bytes_eq_p(int type, object o, object p)
{
  if (type != om_type(p) || o->mem.size != p->mem.size) return FALSE;
  return memcmp(o->mem.elt, p->mem.elt, o->mem.size) == 0;
}

static int array_eq_p(object o, object p)
{
  if (om_type(p) != ARRAY || o->array.size != p->array.size) return FALSE;
  for (int i = 0; i < o->array.size; i++)
    if (!om_eq_p(o->array.elt[i], p->array.elt[i])) return FALSE;
  return TRUE;
}

static int dict_eq_p(object o, object p)
{
  if (om_type(p) != DICT) return FALSE;
  if (o->map.entry_count != p->map.entry_count) return FALSE;
  object keys = om_map_keys(o);
  while (keys != om_nil) {
    object v = om_map_get(p, keys->cons.car);
    if (v == NULL || !om_eq_p(om_map_get(o, keys->cons.car), v)) return FALSE;
    keys = keys->cons.cdr;
  }
  return TRUE;
}

int om_eq_p(object o, object p)
{
  if (o == p) return TRUE;
  switch (om_type(o)) {
    case SINT: return int64_eq_p(om_sint_val(o), p);
    case XINT: return int64_eq_p(o->xint.val, p);
    case XFLOAT: return double_eq_p(o->xfloat.val, p);
    case BYTES: return bytes_eq_p(BYTES, o, p);
    case STRING: return bytes_eq_p(STRING, o, p);
    case CONS: return cons_eq_p(o, p);
    case ARRAY: return array_eq_p(o, p);
    case DICT: return dict_eq_p(o, p);
    default: return FALSE;
  }
}

int om_list_len(object o)
{
  xassert(om_list_p(o));
  int i = 0;
  while (o != om_nil) {
    o = o->cons.cdr;
    i++;
  }
  return i;
}

object om_list_reverse(object o)
{
  xassert(om_list_p(o));
  object head = om_nil;
  while (o != om_nil) {
    object p = o->cons.cdr;
    o->cons.cdr = head;
    head = o;
    o = p;
  }
  return head;
}

object om_map_get(object o, object s)
{
  if (o->map.half_size != 0) {
    object p;
    int i = om_hash(s) % o->map.half_size;
    while ((p = o->map.table[i]) != NULL) {
      if (om_eq_p(p, s)) return o->map.table[i + o->map.half_size];
      if (++i == o->map.half_size) i = 0;
    }
  }
  return NULL;
}

object om_map_get_propagation(object o, object s)
{
  object p;
  while (o != om_nil) {
    if ((p = om_map_get(o, s)) != NULL) return p;
    o = o->map.top;
  }
  return NULL;
}

object om_map_keys(object o)
{
  xassert(om_type(o) == DICT);
  object *table = o->map.table;
  object keys = om_nil;
  for (int i = 0; i < o->map.half_size; i++)
    if (table[i] != NULL) keys = om_new_cons(table[i], keys);
  return keys;
}

static void rehash(object o)
{
  int half_size = o->map.half_size;
  object *table = o->map.table;
  o->map.entry_count = 0;
  o->map.half_size *= 2;
  o->map.table = om_alloc(sizeof(object) * o->map.half_size * 2);
  for (int i = 0; i < o->map.half_size; i++) o->map.table[i] = NULL;
  for (int i = 0; i < half_size; i++)
    if (table[i] != NULL) om_map_put(o, table[i], table[i + half_size]);
  om_free0(sizeof(object) * half_size * 2, table);
}

void om_map_put(object o, object s, object v)
{
  object p;
  xassert(o->map.half_size != 0);
  int i = om_hash(s) % o->map.half_size;
  while ((p = o->map.table[i]) != NULL) {
    if (om_eq_p(p, s)) {
      o->map.table[i + o->map.half_size] = v;
      return;
    }
    if (++i == o->map.half_size) i = 0;
  }
  o->map.table[i] = s;
  o->map.table[i + o->map.half_size] = v;
  o->map.entry_count++;
  if (o->map.entry_count * 2 > o->map.half_size) rehash(o);
}

void om_map_put_propagation(object o, object s, object v)
{
  while (o != om_toplevel) {
    if (om_map_get(o, s) != NULL) {
      om_map_put(o, s, v);
      return;
    }
    o = o->map.top;
  }
  om_map_put(om_toplevel, s, v);
}

void om_map_foreach(object o, void (*f)(void *s, void *v))
{
  object *table = o->map.table;
  for (int i = 0; i < o->map.half_size; i++)
    if (table[i] != NULL) (*f)(table[i], table[i + o->map.half_size]);
}

// initializer.

void om_init(void)
{
  used_memory = 0;
  heap_init(&heap);
  heap_link0 = heap_link1 = NULL;
  symbol_table_init(&st);
  xarray_init(&table0);
  xarray_init(&table1);
  table = &table0;
  table_wk = &table1;
}
