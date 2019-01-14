// basic builtin

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

PRIM(gensym)
{
  static long index = 0;
  char buf[MAX_STR_LEN];
  if (argc != 0) return FALSE;
  sprintf(buf, "$G%ld", index++);
  *result = gc_new_symbol(stralloc(buf));
  return TRUE;
}

PRIM(samep)
{
  int b;
  object o;
  if (argc < 2) return FALSE;
  o = argv->cons.car;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (!(b = (o == argv->cons.car))) break;
  }
  *result = object_bool(b);
  return TRUE;
}

static int equal_s_expr(object o, object p);
static int equal_cons_p(object o, object p)
{
  return equal_s_expr(o->cons.car, p->cons.car)
    && equal_s_expr(o->cons.cdr, p->cons.cdr);
}

static int equal_s_expr(object o, object p)
{
  double x, y;
  if (o == p) return TRUE;
  if (bi_double(o, &x) && bi_double(p, &y) && x == y) return TRUE;
  if (typep(o, CONS) && typep(p, CONS)) return equal_cons_p(o, p);
  return FALSE;
}

// compare built-in object.
PRIM(equalp)
{
  int b;
  object o, p;
  if (argc < 2) return FALSE;
  o = argv->cons.car;
  while ((argv = argv->cons.cdr) != object_nil) {
    p = argv->cons.car;
    if (!(b = equal_s_expr(o, p))) break;
  }
  *result = object_bool(b);
  return TRUE;
}

PRIM(atom_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(!typep(argv->cons.car, CONS));
  return TRUE;
}

PRIM(number_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(numberp(argv->cons.car));
  return TRUE;
}

PRIM(symbol_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, SYMBOL));
  return TRUE;
}

PRIM(keyword_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, KEYWORD));
  return TRUE;
}

PRIM(lambda_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, LAMBDA));
  return TRUE;
}

PRIM(macro_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, MACRO));
  return TRUE;
}

PRIM(byte_array_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(typep(argv->cons.car, BARRAY));
  return TRUE;
}

static int special_p(object o)
{
  int i;
  char *s, *t;
  if (!typep(o, SYMBOL)) return FALSE;
  s = o->symbol.name;
  for (i = 0; (t = bi_as_symbol_name(special_name_table[i])) != NULL; i++) {
    if (strcmp(s, t) == 0) return TRUE;
  }
  return FALSE;
}

static int prim_p(object o)
{
  int i;
  char *s, *t;
  if (!typep(o, SYMBOL)) return FALSE;
  s = o->symbol.name;
  for (i = 0; (t = bi_as_symbol_name(prim_name_table[i])) != NULL; i++) {
    if (strcmp(s, t) == 0) return TRUE;
  }
  return FALSE;
}

PRIM(special_operator_p)
{
  if (argc != 1) return FALSE;
  *result = object_bool(special_p(argv->cons.car));
  return TRUE;
}

PRIM(operator_p)
{
  object o;
  if (argc != 1) return FALSE;
  o = argv->cons.car;
  *result = object_bool(special_p(o) || prim_p(o) || typep(o, LAMBDA));
  return TRUE;
}

// TODO should be removed
PRIM(print)
{
  char buf[MAX_STR_LEN];
  if (argc != 1) return FALSE;
  *result = argv->cons.car;
  printf("%s\n", object_describe(*result, buf));
  return TRUE;
}

// TODO should be removed
PRIM(quit)
{
  printf("paren exit");
  exit(1);
}
