// paren main routine.

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "lex.h"
#include "ip.h"

// option

static int dump_object_table_p;
static char *core_fn;

static void parse_opt(int argc,char *argv[])
{
  core_fn = "./core.p";
  if (argc == 2) core_fn = argv[1];
  dump_object_table_p = FALSE;
}

// object construct and regist

static struct xsplay symbol_table;

static int symcmp(object o, object p)
{
  xassert(o->header.type == symbol && o->header.type == p->header.type);
  return strcmp(o->symbol.name, p->symbol.name);
}

static void regist(object o)
{
  xarray_add(&object_table, o);
}

static object new_cons(object car, object cdr)
{
  object o;
  o = object_alloc();
  o->header.type = cons;
  o->cons.car = car;
  o->cons.cdr = cdr;
  regist(o);
  return o;
}

// object object_new_barray(int len)
// {
//   object o;
//   o = xmalloc(sizeof(struct fbarray) + len - 1);
//   o->header.type = fbarray;
//   return o;
// }
//
// object object_new_fbarray(int len)
// {
//   object o;
//   o = xmalloc(sizeof(struct farray) + (len - 1) * sizeof(object));
//   o->header.type = farray;
//   while (len-- > 0) o->farray.elt[len] = object_nil;
//   return o;
// }

static object new_xint(int val)
{
  object o;
  o = object_alloc();
  o->header.type = xint;
  o->xint.val = val;
  regist(o);
  return o;
}

static object new_xfloat(double val)
{
  object o;
  o = object_alloc();
  o->header.type = xfloat;
  o->xfloat.val = val;
  regist(o);
  return o;
}

static object new_symbol(char *name)
{
  object o;
  if ((o = xsplay_find(&symbol_table, name)) == NULL) {
    o = object_alloc();
    if (name[0] != ':') o->header.type = symbol;
    else o->header.type = keyword;
    o->symbol.name = name;
    xsplay_add(&symbol_table, name, o);
    regist(o);
  }
  return o;
}

// parser

static int next_token;

static int parse_skip(void)
{
  return next_token = lex();
}

static object parse_s_expr(void);

static object parse_symbol(void)
{
  char *s;
  s = stralloc(lex_str.elt);
  parse_skip();
  return new_symbol(s);
}

static object parse_integer(void)
{
  int val;
  val = lex_ival;
  parse_skip();
  return new_xint(val);
}

static object parse_float(void)
{
  double val;
  val = lex_fval;
  parse_skip();
  return new_xfloat(val);
}

static object parse_cdr(void)
{
  object cdr;
  if (next_token == ')') return object_nil;
  if (next_token == '.') {
    parse_skip();
    cdr = parse_s_expr();
    if (next_token != ')') lex_error("illegal dot list");
    return cdr;
  }
  return new_cons(parse_s_expr(), parse_cdr());
}

static object parse_list(void)
{
  object o;
  if (parse_skip() == ')') o = object_nil;
  else o = new_cons(parse_s_expr(), parse_cdr());
  parse_skip();
  return o;
}

static object parse_atom(void)
{
  switch (next_token) {
    case LEX_INT: return parse_integer();
    case LEX_FLOAT: return parse_float();
    case LEX_SYMBOL: return parse_symbol();
    default: lex_error("illegal token value '%d'.", next_token); return NULL;
  }
}

static object parse_s_expr(void)
{
  if (next_token == '(') return parse_list();
  return parse_atom();
}

// loader

static object load_rec(void)
{
  if (next_token == EOF) return object_nil;
  return new_cons(parse_s_expr(), load_rec());
}

static object load(char *fn)
{
  FILE *fp;
  object o;
  if ((fp = fopen(fn, "r")) == NULL) xerror("load/open %s failed.", fn);
  lex_start(fp);
  parse_skip();
  o = load_rec();
  fclose(fp);
  return o;
}

extern int (*prim_table[])(object *args, object *result);
extern char *prim_name_table[];

static void make_initial_objects(void)
{
  object_nil = new_symbol("nil");
  object_true = new_symbol("true");
  object_false = new_symbol("false");
}

static void bind_prim(object o)
{
  int i;
  char *s;
  object p;
  for (i = 0; (s = prim_name_table[i]) != NULL; i++) {
    p = object_alloc();
    p->header.type = lambda;
    p->lambda.top = p->lambda.params = p->lambda.body = object_nil;
    p->lambda.prim_cd = i;
    xsplay_add(&o->lambda.binding, new_symbol(s), p);
  }
}

static void bind_pseudo_symbol(object o, object p)
{
  xsplay_add(&o->lambda.binding, p, p);
}

static object make_boot_args(object o)
{
  object boot_arg;
  boot_arg = object_alloc();
  boot_arg->header.type = lambda;
  boot_arg->lambda.top = boot_arg->lambda.params = object_nil;
  boot_arg->lambda.body = o;
  boot_arg->lambda.prim_cd = -1;
  xsplay_init(&boot_arg->lambda.binding, (int(*)(void *, void *))symcmp);
  bind_pseudo_symbol(boot_arg, object_nil);
  bind_pseudo_symbol(boot_arg, object_true);
  bind_pseudo_symbol(boot_arg, object_false);
  bind_prim(boot_arg);
  regist(boot_arg);
  return boot_arg;
}

int main(int argc, char *argv[])
{
  int i;
  object boot_arg;
  setbuf(stdout, NULL);
  parse_opt(argc, argv);
  object_init();
  xsplay_init(&symbol_table, (int(*)(void *, void *))strcmp);
  make_initial_objects();
  boot_arg = make_boot_args(load(core_fn));
  if (dump_object_table_p) {
    printf("* object table >\n");
    for(i = 0; i < object_table.size; i++) object_dump(object_table.elt[i]);
    printf("<\n");
  }
  ip_start(boot_arg);
  return 0;
}
