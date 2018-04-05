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
  core_fn = "core.p";
  dump_object_table_p = TRUE;
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

static object new_lambda(object params, object body, int prim_cd)
{
  object o, p;
  o = object_alloc();
  o->header.type = lambda;
  o->lambda.top = object_nil;
  o->lambda.params = params;
  o->lambda.body = body;
  o->lambda.prim_cd = prim_cd;
  xsplay_init(&o->lambda.binding, (int(*)(void *, void *))symcmp);
  while (params->header.type == cons) {
    if ((p = params->cons.car)->header.type == symbol)
      xsplay_add(&o->lambda.binding, p, object_nil);
    params = params->cons.cdr;
  }
  regist(o);
  return o;
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

static object new_symbol_keyword(int token_kind, char *name)
{
  object o;
  if ((o = xsplay_find(&symbol_table, name)) == NULL) {
    o = object_alloc();
    if (token_kind == LEX_SYMBOL) o->header.type = symbol;
    else o->symbol.name = stralloc(name);
    xsplay_add(&symbol_table, o->symbol.name, o);
  }
  regist(o);
  return o;
}

static object new_symbol(char *name)
{
  return new_symbol_keyword(LEX_SYMBOL, name);
}

// parser

static int next_token;

static int parse_skip(void)
{
  return next_token = lex();
}

static object parse_s_expr(void);

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
  parse_skip();    // skip '('
  if (next_token == ')') o = object_nil;
  else o = new_cons(parse_s_expr(), parse_cdr());
  parse_skip();    // skip ')'
  return o;
}

static object parse_symbol_keyword(void)
{
  object o;
  o = new_symbol_keyword(next_token, lex_str.elt);
  parse_skip();
  return o;
}

static object parse_integer(void)
{
  int val = lex_ival;
  parse_skip();
  return new_xint(val);
}

static object parse_float(void)
{
  double val = lex_fval;
  parse_skip();
  return new_xfloat(val);
}

static object parse_s_expr(void)
{
  switch (next_token) {
    case '(': return parse_list();
    case LEX_INT: return parse_integer();
    case LEX_FLOAT: return parse_float();
    case LEX_SYMBOL:
    case LEX_KEYWORD: return parse_symbol_keyword();
    default: lex_error("illegal token value '%d'.", next_token); return NULL;
  }
}

// loader

static void load(char *fn)
{
  FILE *fp;
  if((fp = fopen(fn, "r")) == NULL) xerror("load/open %s failed.", fn);
  lex_start(fp);
  parse_skip();
  while (next_token != EOF) parse_s_expr();
  fclose(fp);
}

static char *prim_name_table[]={
#define PRIM(n) #n,
#include "prim_name.wk"
#undef PRIM
  NULL
};

static void make_initial_objects(void)
{
  int i;
  char *s;
  object_nil = new_symbol("nil");
  object_true = new_symbol("true");
  object_false = new_symbol("false");
  object_env = new_lambda(object_nil, object_nil, -1);
  for (i = 0; (s = prim_name_table[i]) != NULL; i++) new_symbol(s);
}

static object *make_boot_args(void)
{
  object *boot_args;
  boot_args = xmalloc(sizeof(object) * 4);
  boot_args[0] = object_nil;
  boot_args[1] = object_true;
  boot_args[2] = object_false;
  boot_args[3] = object_env;
  return boot_args;
}

int main(int argc, char *argv[])
{
  int i;
  object *boot_args;
  setbuf(stdout, NULL);
  parse_opt(argc, argv);
  object_init();
  xsplay_init(&symbol_table, (int(*)(void *, void *))strcmp);
  make_initial_objects();
  load(core_fn);
  if (dump_object_table_p)
    for(i = 0; i < object_table.size; i++) object_dump(object_table.elt[i]);
  boot_args = make_boot_args();
  ip_start(boot_args);
  return 0;
}
