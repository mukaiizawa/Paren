// paren main routine.

#include "std.h"
#include "xgetopt.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "lex.h"
#include "gc.h"
#include "ip.h"

extern char *prim_name_table[];

// option

static int dump_object_table_p;
static int silentp;
static char *core_fn;

static void parse_opt(int argc,char *argv[])
{
  int ch;
  core_fn = "./core.p";
  silentp = FALSE;
  dump_object_table_p = FALSE;
  while((ch = xgetopt(argc,argv,"sof:")) != EOF)
    switch(ch) {
      case 's': silentp = TRUE; break;
      case 'o': dump_object_table_p = TRUE; break;
      case 'f': core_fn = xoptarg; break;
      default: xerror("\
-o dump object table.\n\
-s silent mode.\n\
-f FILE as core library for paren\n\
");
    }
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
  object o;    // function parameters are not evaluated in a defined order in C
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

static void bind_pseudo_symbol(object o)
{
  xsplay_add(&toplevel->lambda.binding, o, o);
}

static void bind_prim(void)
{
  int i;
  char *s;
  object p, q;
  for (i = 0; (s = prim_name_table[i]) != NULL; i++) {
    p = gc_new_symbol(s); 
    q = gc_new_lambda(toplevel, object_nil, object_nil, i);
    xsplay_add(&toplevel->lambda.binding, p, q);
  }
}

static void make_initial_objects(void)
{
  object_nil = gc_new_symbol("nil");
  object_true = gc_new_symbol("true");
  object_false = gc_new_symbol("false");
  object_opt = gc_new_symbol(":opt");
  object_key = gc_new_symbol(":key");
  object_rest = gc_new_symbol(":rest");
  toplevel = gc_new_lambda(object_nil, object_nil, object_nil, -1);
  // TODO >>
  xsplay_add(&toplevel->lambda.binding, gc_new_symbol("-1")
      , gc_new_xint(-1));
  // <<
  bind_pseudo_symbol(object_nil);
  bind_pseudo_symbol(object_true);
  bind_pseudo_symbol(object_false);
  bind_prim();
}

int main(int argc, char *argv[])
{
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
  parse_opt(argc, argv);
  gc_init();
  make_initial_objects();
  toplevel->lambda.body = load(core_fn);
  ip_start(toplevel, silentp);
  if (dump_object_table_p) gc_dump_table();
  gc_full();
  if (dump_object_table_p) gc_dump_table();
  return 0;
}
