// paren main routine.

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "lex.h"

/*
 * paren bnf
 * <s_expr> ::= <list> | <atom>
 * <list> ::= <pure_list> | <dot_list>
 * <pure_list> ::= '(' [<s_expr>] ... ')'
 * <dot_list> ::= '(' <s_expr> ... '.' <s_expr> ')'
 * <atom> ::= <number> | <symbol> | <keyword>
 * <number> ::= [<digit>+ 'x'] [0-9a-z]+ | <digit>+ [ '.' <digit>+ ]
 * <symbol> ::= <identifier>
 * <keyword> ::= ':' <identifier>
 * <identifier> ::= <identifier_first> [<identifier_rest>] ...
 * <identifier_first> ::= [!$%&*+-/\-<=>?a-zA-Z_]
 * <identifier_rest> ::= <identifier_first> | [0-9]
 * <digit> ::= [0-9]
 */

// symbol table and environment

static struct xsplay prim_table;
static struct xsplay symbol_table;
static struct xsplay keyword_table;
static object toplevel;
static object env;

static int symcmp(object o, object p)
{
  return strcmp(o->symbol.name, p->symbol.name);
}

static char *prim_name_table[] = {
#define PRIM(n) #n,
#include "prim.wk"
#undef PRIM
  NULL
};

static object find(object sym)
{
  object o, e;
  xassert(object_typep(sym, symbol));
  e = env;
  while (e != object_nil) {
    if ((o = xsplay_find(&e->lambda.binding, sym)) != NULL) return o;
    e = e->lambda.top;
  }
  return NULL;
}

static void bind(object sym, object val)
{
  object e;
  xassert(object_typep(sym, symbol));
  e = env;
  while (e != object_nil) {
    if (xsplay_find(&e->lambda.binding, sym) != NULL) {
      xsplay_delete(&e->lambda.binding, sym);
      xsplay_add(&e->lambda.binding, sym, val);
      return;
    }
    e = e->lambda.top;
  }
  xsplay_add(&toplevel->lambda.binding, sym, val);
}

static void dump_symbol(int depth, void *key, void *data)
{
  printf("\t%s\t%p\n", key, data);
}

static void dump_symbol_table()
{
  printf("symbol_table {\n");
  xsplay_foreach(&symbol_table, dump_symbol);
  xsplay_foreach(&prim_table, dump_symbol);
  printf("}\n");
}

// object construct and regist

static object object_alloc()
{
  return xmalloc(sizeof(union s_expr));
}

static object new_lambda(object top, object params, object body)
{
  object o;
  o = object_alloc();
  o->header.type = lambda;
  o->lambda.top = top;
  o->lambda.params = params;
  o->lambda.body = body;
  xsplay_init(&o->lambda.binding, (int(*)(void* ,void*))symcmp);
  return o;
}

static object new_cons(object car, object cdr)
{
  object o;
  o = object_alloc();
  o->header.type = cons;
  o->cons.car = car;
  o->cons.cdr = cdr;
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
  return o;
}

static object new_xfloat(double val)
{
  object o;
  o = object_alloc();
  o->header.type = xfloat;
  o->xfloat.val = val;
  return o;
}

static object new_symbol(char *name)
{
  object o;
  if ((o = xsplay_find(&symbol_table, name)) == NULL) {
    o = object_alloc();
    o->header.type = symbol;
    o->symbol.name = stralloc(name);
    xsplay_add(&symbol_table, o->symbol.name, o);
  }
  return o;
}

static object new_keyword(char *name)
{
  object o;
  if ((o = xsplay_find(&keyword_table, name)) == NULL) {
    o = object_alloc();
    o->header.type = keyword;
    o->symbol.name = stralloc(name);
    xsplay_add(&keyword_table, o->symbol.name, o);
  }
  return o;
}

static void make_initial_objects(void)
{
  int i;
  char *s;
  xsplay_init(&prim_table, (int(*)(void* ,void*))strcmp);
  xsplay_init(&symbol_table, (int(*)(void* ,void*))strcmp);
  xsplay_init(&keyword_table, (int(*)(void* ,void*))strcmp);
  object_nil = new_symbol("nil");
  object_true = new_symbol("true");
  object_false = new_symbol("false");
  env = toplevel = new_lambda(object_nil, object_nil, object_nil);
  bind(object_nil, object_nil);
  bind(object_true, object_true);
  bind(object_false, object_false);
  for (i = 0; (s = prim_name_table[i]) != NULL; i++)
    xsplay_add(&prim_table, s, new_symbol(s));
}

// parser

static int next_token;

static int parse_skip(void)
{
  return next_token = lex();
}

static object parse_s_expr(void);

static object parse_cdr()
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

static object parse_symbol(void)
{
  object o;
  o = new_symbol(lex_str.elt);
  parse_skip();
  return o;
}

static object parse_keyword(void)
{
  object o;
  o = new_keyword(lex_str.elt);
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
    case LEX_SYMBOL: return parse_symbol();
    case LEX_KEYWORD: return parse_keyword();
    default: lex_error("illegal token value '%d'.", next_token); return NULL;
  }
}

// evaluater

static object eval_cons(object o)
{
  if (!object_typep(object_car(o), symbol)) {
    object_dump(o);
    lex_error("illegal list");
  }
  return o;
}

static object eval(object o)
{
  object p;
  switch (o->header.type) {
    case lambda:
    case fbarray:
    case farray:
    case xint:
    case xfloat:
    case keyword:
      return o;
    case symbol:
      if ((p = find(o)) == NULL)
        lex_error("unbind symbol '%s'", o->symbol.name);
      return p;
    case cons:
      return eval_cons(o);
  }
}

// printer

static void print(object o)
{
  object_dump(o);
}

// loader

static void load(char *fn)
{
  FILE *fp;
  if((fp = fopen(fn, "r")) == NULL) xerror("load/open %s failed.", fn);
  lex_start(fp);
  parse_skip();
  while (next_token != EOF) print(eval(parse_s_expr()));
  fclose(fp);
}

int main(int argc, char *argv[])
{
  char *core_fn;
  setbuf(stdout, NULL);
  core_fn = "core.p";
  make_initial_objects();
  load(core_fn);
  dump_symbol_table();
  return 0;
}
