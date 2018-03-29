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

static int typep(object o, enum object_type type)
{
  return o->header.type == type;
}

static object reverse(object o)
{
  object p;
  xassert(object_listp(o));
  p = object_nil;
  while (!object_nilp(o)) {
    p = object_new_cons(o->cons.car, p);
    o = o->cons.cdr;
  }
  return p;
}

// object construct and regist

static object toplevel;
static struct xsplay symbol_table;

static int symbol_cmp(object o, object p)
{
  xassert(typep(o, symbol) && typep(p, symbol));
  return strcmp(o->symbol.name, p->symbol.name);
}

// static object find(object env, object sym)
// {
//   object o;
//   xassert(typep(env, lambda) && typep(sym, symbol));
//   while (env != object_nil) {
//     if ((o = xsplay_find(&env->lambda.binding, sym)) != NULL) return o;
//     env = env->lambda.top;
//   }
//   return object_nil;
// }

static void bind(object env, object sym, object val)
{
  xassert(typep(env, lambda) && typep(sym, symbol));
  while (env != object_nil) {
    if (xsplay_find(&env->lambda.binding, sym) != NULL) {
      xsplay_delete(&env->lambda.binding, sym);
      xsplay_add(&env->lambda.binding, sym, val);
      return;
    }
    env = env->lambda.top;
  }
  xsplay_add(&toplevel->lambda.binding, sym, val);
}

static object new_lambda(object top, object params, object body)
{
  object o;
  o = object_alloc();
  o->header.type = lambda;
  o->lambda.params = params;
  o->lambda.body = body;
  xsplay_init(&o->lambda.binding, (int(*)(void* ,void*))symbol_cmp);
  return o;
}

static object new_keyword(char *name)
{
  object o;
  if ((o = xsplay_find(&symbol_table, name)) == NULL) {
    o = object_alloc();
    o->header.type = keyword;
    o->symbol.name = stralloc(name);
  }
  return o;
}

static void make_initial_objects(void)
{
  xsplay_init(&symbol_table, (int(*)(void* ,void*))symbol_cmp);
  object_nil = object_alloc();
  object_nil->header.type = symbol;
  object_nil->symbol.name = stralloc("nil");
  toplevel = new_lambda(object_nil, object_nil, object_nil);
  bind(toplevel, object_nil, object_nil);
  object_true = object_new_symbol("true");
  bind(toplevel, object_true, object_nil);
  object_false = object_new_symbol("false");
  bind(toplevel, object_false, object_false);
}

// parser

static int next_token;

static int parse_skip(void)
{
  return next_token = lex();
}

static object parse_s_expr(void);

static object parse_list(void)
{
  object o;
  o = object_nil;
  parse_skip();    // skip '('
  while (next_token != ')') o = object_new_cons(parse_s_expr(), o);
  parse_skip();    // skip ')'
  return reverse(o);
}

static object parse_symbol(void)
{
  object o;
  o = object_new_symbol(lex_str.elt);
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
  return object_new_xint(val);
}

static object parse_float(void)
{
  double val = lex_fval;
  parse_skip();
  return object_new_xfloat(val);
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

static object eval(object o)
{
  object p;
  switch (o->header.type) {
    case lambda:
    case cons:
    case fbarray:
    case farray:
    case xint:
    case xfloat:
    case keyword:
    case symbol:
      p = o;
      break;
  }
  return p;
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
  return 0;
}
