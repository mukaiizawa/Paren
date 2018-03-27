// paren main routine.

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

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
  o = object_new_keyword(lex_str.elt);
  parse_skip();
  return o;
}

static object parse_integer(void)
{
  int val = lex_ival;
  parse_skip();
  return object_new_xfloat(val);
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
    case LEX_SYMBOL: return parse_symbol();
    case LEX_KEYWORD: return parse_keyword();
    case LEX_INT: return parse_integer();
    case LEX_FLOAT: return parse_float();
    default: lex_error("illegal token value '%d'.", next_token); return NULL;
  }
}

static object eval(object o)
{
  object_dump(o);
  return o;
}

static void make_initial_objects(void)
{
  object_nil = object_new_symbol("nil");
  object_nil->symbol.val = object_nil;
  object_true = object_new_symbol("true");
  object_false = object_new_symbol("false");
}

static void load(char *fn)
{
  FILE *fp;
  if((fp = fopen(fn, "r")) == NULL) xerror("load/open %s failed.", fn);
  lex_start(fp);
  parse_skip();
  while (next_token != EOF) eval(parse_s_expr());
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
