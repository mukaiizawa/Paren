// paren main routine.

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "lex.h"
#include "gc.h"
#include "ip.h"

// option

static int dump_object_table_p;
static char *core_fn;

int symcmp(object o, object p);

static void parse_opt(int argc,char *argv[])
{
  core_fn = "./core.p";
  if (argc == 2) core_fn = argv[1];
  dump_object_table_p = FALSE;
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
  return gc_new_symbol(s);
}

static object parse_integer(void)
{
  int val;
  val = lex_ival;
  parse_skip();
  return gc_new_xint(val);
}

static object parse_float(void)
{
  double val;
  val = lex_fval;
  parse_skip();
  return gc_new_xfloat(val);
}

static object parse_cdr(void)
{
  object o;
  if (next_token == ')') return object_nil;
  if (next_token == '.') {
    parse_skip();
    o = parse_s_expr();
    if (next_token != ')') lex_error("illegal dot list");
    return o;
  }
  o = parse_s_expr();
  return gc_new_cons(o, parse_cdr());
}

static object parse_list(void)
{
  object o;
  if (parse_skip() == ')') o = object_nil;
  else {
    o = parse_s_expr();
    o = gc_new_cons(o, parse_cdr());
  }
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
  object o;    // function parameters are not evaluated in a defined order in C!
  if (next_token == EOF) return object_nil;
  o = parse_s_expr();
  return gc_new_cons(o, load_rec());
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

extern char *prim_name_table[];

static void make_initial_objects(void)
{
  object_nil = gc_new_symbol("nil");
  object_true = gc_new_symbol("true");
  object_false = gc_new_symbol("false");
  object_opt = gc_new_symbol(":opt");
  object_key = gc_new_symbol(":key");
  object_rest = gc_new_symbol(":rest");
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
    xsplay_add(&o->lambda.binding, gc_new_symbol(s), p);
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
  gc_regist(boot_arg);
  return boot_arg;
}

int main(int argc, char *argv[])
{
  object boot_arg;
  setbuf(stdout, NULL);
  parse_opt(argc, argv);
  gc_init();
  make_initial_objects();
  boot_arg = make_boot_args(load(core_fn));
  if (dump_object_table_p) gc_dump_table();
  ip_start(boot_arg);
  gc_full();
  return 0;
}
