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
  "atom_p", "atom?",
  "barray_new", "byte-array",
  "barray_to_symbol_keyword", "byte-array->symbol/keyword",
  "byte_array_p", "byte-array?",
  "equalp", "=",
  "keyword_p", "keyword?",
  "keyword_to_barray", "keyword->byte-array",
  "macro_p", "macro?",
  "number_add", "+",
  "number_lt", "<",
  "number_modulo", "mod",
  "number_multiply", "*",
  "number_p", "number?",
  "operator_p", "operator?",
  "os_fgetc", "OS._fgetc",
  "os_fgets", "OS._fgets",
  "os_fopen", "OS._fopen",
  "os_fp", "OS._fp",
  "os_fputc", "OS._fputc",
  "samep", "same?",
  "special_operator_p", "special-operator?",
  "symbol_p", "symbol?",
  "symbol_to_barray", "symbol->byte-array",
  "throw", "basic-throw",
  "try", "basic-try",
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

int bi_int64(object o, int64_t *p) {
  if (!typep(o, XINT)) return FALSE;
  *p = o->xint.val;
  return TRUE;
}

int bi_intptr(object o, intptr_t *p) {
  int64_t i;
  if (!bi_int64(o, &i)) return FALSE;
  if (i < INTPTR_MIN || i > INTPTR_MAX) return FALSE;
  *p = (intptr_t)i;
  return TRUE;
}

int bi_double(object o, double *p) {
  int64_t i;
  if (bi_int64(o, &i)) *p = (double)i;
  else if (typep(o, XFLOAT)) *p = o->xfloat.val;
  else return FALSE;
  return TRUE;
}
