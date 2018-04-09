// primitive table for paren

#include "std.h"
#include "xsplay.h"
#include "object.h"

#define PRIM(name) extern int prim_##name(object *args, object *result);
#include "pprim.wk"
#undef PRIM

int (*prim_table[])(object *args, object *result) = {
#define PRIM(name) prim_##name,
#include "pprim.wk"
#undef PRIM
  NULL
};
