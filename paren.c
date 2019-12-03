// paren main routine.

#include "std.h"
#include "xgetopt.h"
#include "xsplay.h"
#include "xarray.h"
#include "pf.h"
#include "object.h"
#include "lex.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

// option

char *core_fn;

static void parse_opt(int argc, char *argv[])
{
  int ch;
  core_fn = "./core.p";
  while((ch = xgetopt(argc, argv, "f:o")) != EOF)
    switch(ch) {
      case 'f': core_fn = xoptarg; break;
      default: xerror("\
-f FILE as core library for paren\n\
");
    }
}

// parser

static int next_token;
struct xbarray token_str;

static int parse_skip(void)
{
  return next_token = lex();
}

static int parse_token(int token)
{
  char buf[MAX_STR_LEN];
  if (next_token != token) lex_error("missing %s", lex_token_name(buf, token));
  switch (token) {
    case LEX_SYMBOL:
    case LEX_KEYWORD:
    case LEX_STRING:
      xbarray_copy(&token_str, &lex_str);
      break;
    default:
      break;
  }
  return parse_skip();
}

static object parse_symbol(void)
{
  parse_token(LEX_SYMBOL);
  return gc_new_barray_from(SYMBOL, token_str.size, token_str.elt);
}

static object parse_keyword(void)
{
  parse_token(LEX_KEYWORD);
  return gc_new_barray_from(KEYWORD, token_str.size, token_str.elt);
}

static object parse_string(void)
{
  parse_token(LEX_STRING);
  return gc_new_barray_from(STRING, token_str.size, token_str.elt);
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

static object parse_atom(void)
{
  switch (next_token) {
    case LEX_SYMBOL: return parse_symbol();
    case LEX_KEYWORD: return parse_keyword();
    case LEX_STRING: return parse_string();
    case LEX_INT: return parse_integer();
    case LEX_FLOAT: return parse_float();
    default: lex_error("illegal token '%c'.", (char)next_token); return NULL;
  }
}

static object parse_s_expr(void);

static object parse_cdr(void)
{
  object o;
  if (next_token == ')') return object_nil;
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
  parse_token(')');
  return o;
}

static object parse_s_expr(void)
{
  object o;
  if (next_token == '\'') {
    o = object_quote;
    parse_skip();
    return gc_new_cons(o, gc_new_cons(parse_s_expr(), object_nil));
  }
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

static object load(void)
{
  FILE *fp;
  object o;
  if ((fp = fopen(core_fn, "r")) == NULL)
    xerror("load/open %s failed.", core_fn);
  lex_start(fp);
  parse_skip();
  o = load_rec();
  fclose(fp);
  return o;
}

static void bind_symbol(object k, object v)
{
  object o;
  o = object_toplevel->env.binding;
  object_toplevel->env.binding = gc_new_cons(k , gc_new_cons(v, o));
}

static void bind_pseudo_symbol(object o)
{
  bind_symbol(o, o);
}

static object symbol_new(char *name)
{
  return gc_new_barray_from(SYMBOL, strlen(name), name);
}

static object keyword_new(char *name)
{
  return gc_new_barray_from(KEYWORD, strlen(name), name);
}

static void bind_special(void)
{
  int i;
  char *s;
  object o;
  xsplay_init(&special_splay, (int(*)(void *, void *))symcmp);
  xsplay_init(&prim_splay, (int(*)(void *, void *))symcmp);
  for (i = 0; (s = bi_as_symbol_name(special_name_table[i])) != NULL; i++) {
    bind_pseudo_symbol(o = symbol_new(s));
    xsplay_add(&special_splay, o, special_table[i]);
  }
  for (i = 0; (s = bi_as_symbol_name(prim_name_table[i])) != NULL; i++) {
    bind_pseudo_symbol(o = symbol_new(s));
    xsplay_add(&prim_splay, o, prim_table[i]);
  }
}

static void make_initial_objects(void)
{
  int i;
  object_nil = symbol_new("nil");
  object_key = keyword_new("key");
  object_opt = keyword_new("opt");
  object_quote = symbol_new("quote");
  object_rest = keyword_new("rest");
  object_toplevel = gc_new_env(object_nil);
  object_true = symbol_new("true");
  object_os = symbol_new("$os");
#if WINDOWS_P
  bind_symbol(object_os, keyword_new("Windows"));
#elif OS_CODE == OS_LINUX
  bind_symbol(object_os, keyword_new("Linux"));
#elif OS_CODE == OS_ANDROID
  bind_symbol(object_os, keyword_new("Android"));
#elif OS_CODE == OS_MACOSX
  bind_symbol(object_os, keyword_new("Mac"));
#else
  xassert(FALSE);
#endif
  for (i = 0; i < 256; i++) object_bytes[i] = gc_new_bytes(i);
  bind_pseudo_symbol(object_nil);
  bind_pseudo_symbol(object_true);
  bind_special();
}

int main(int argc, char *argv[])
{
  char buf[MAX_STR_LEN];
  setbuf(stdout, NULL);
  parse_opt(argc, argv);
  if (core_fn == NULL) {
    pf_exepath(argv[0], buf);
#if !UNIX_P
    *strrchr(buf, '.') = '\0';
#endif
    strcat(buf, ".p");
    core_fn = buf;
  }
  gc_init();
  make_initial_objects();
  xbarray_init(&token_str);
  object_boot = gc_new_lambda(object_toplevel, object_nil, load());
  ip_start();
  return 0;
}
