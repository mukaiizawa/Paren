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
  "assign", "<-",
  "try", "basic-try",
  "throw", "basic-throw",
  "samep", "same?",
  "equalp", "=",
  "type", "$$type",
  "number_add", "+",
  "number_multiply", "*",
  "number_lt", "<",
  "number_modulo", "mod",
  "number_ineger_p", "integer?",
  "array_new", "array",
  "barray_new", "byte-array",
  "array_access", "[]",
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
