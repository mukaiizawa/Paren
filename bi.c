// built-in.

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

#undef SPECIAL
#undef PRIM

#define SPECIAL(name) extern void special_##name(object, int, object);
#include "special.wk"
#undef SPECIAL

#define PRIM(name) extern int prim_##name(int, object, object *);
#include "prim.wk"
#undef PRIM

static char *symbol_name_map[] = {
  "array_access", "[]",
  "array_copy", "array-copy",
  "array_size", "array-size",
  "assign", "<-",
  "barray_new", "byte-array",
  "barray_to_symbol_keyword", "byte-array->symbol/keyword",
  "equalp", "=",
  "keyword_to_barray", "keyword->byte-array",
  "number_add", "+",
  "number_ineger_p", "integer?",
  "number_lt", "<",
  "number_modulo", "mod",
  "number_multiply", "*",
  "os_fp", "OS._fp",
  "os_fopen", "OS._fopen",
  "samep", "same?",
  "symbol_to_barray", "symbol->byte-array",
  "throw", "basic-throw",
  "try", "basic-try",
  "type", "$$type",
  NULL
};

char *special_name_table[] = {
#define SPECIAL(name) #name,
#include "special.wk"
#undef SPECIAL
  NULL
};

char *prim_name_table[] = {
#define PRIM(name) #name,
#include "prim.wk"
#undef PRIM
  NULL
};

void (*special_table[])(object env, int argc, object argv) = {
#define SPECIAL(name) special_##name,
#include "special.wk"
#undef SPECIAL
  NULL
};

int (*prim_table[])(int argc, object argv, object *result) = {
#define PRIM(name) prim_##name,
#include "prim.wk"
#undef PRIM
  NULL
};

char *bi_as_symbol_name(char *name)
{
  int i;
  if (name == NULL) return NULL;
  for (i = 0; symbol_name_map[i] != NULL; i += 2)
    if (strcmp(name, symbol_name_map[i]) == 0) return symbol_name_map[i + 1];
  return name;
}

double bi_double_val(object o)
{
  xassert(o != NULL);
  switch (type(o)) {
    case XINT: return (double)o->xint.val;
    case XFLOAT: return o->xfloat.val;
    default: xerror("illegal state");
  }
  return -1;
}

object bi_xint(int64_t val)
{
  if (XINT_MIN <= val && val <= XINT_MAX) return gc_new_xint(val);
  return gc_new_xfloat((double)val);
}

// object bi_intptr_val(int64_t val)
// {
//   if (XINT_MIN <= val && val <= XINT_MAX) return gc_new_xint(val);
//   return gc_new_xfloat((double)val);
// }
