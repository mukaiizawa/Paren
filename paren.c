// paren main routine.

#include "std.h"
#include "xgetopt.h"
#include "xarray.h"
#include "pf.h"
#include "object.h"
#include "lex.h"
#include "gc.h"
#include "splay.h"
#include "ip.h"
#include "bi.h"

static char *core_fn;

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
  return gc_new_barray_from(SYMBOL, token_str.elt, token_str.size);
}

static object parse_keyword(void)
{
  parse_token(LEX_KEYWORD);
  return gc_new_barray_from(KEYWORD, token_str.elt, token_str.size);
}

static object parse_string(void)
{
  parse_token(LEX_STRING);
  return gc_new_barray_from(STRING, token_str.elt, token_str.size);
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
  splay_add(object_toplevel->env.binding, k, v);
}

static void bind_pseudo_symbol(object o)
{
  bind_symbol(o, o);
}

static object symbol_new(char *name)
{
  return gc_new_barray_from(SYMBOL, name, strlen(name));
}

static object keyword_new(char *name)
{
  return gc_new_barray_from(KEYWORD, name, strlen(name));
}

static void make_builtin(void)
{
  int i;
  char *s;
  object o;
  for (i = 0; (s = bi_as_symbol_name(special_name_table[i])) != NULL; i++) {
    o = gc_new_builtin(SPECIAL, symbol_new(s), special_table[i]);
    bind_symbol(o->builtin.name, o);
  }
  for (i = 0; (s = bi_as_symbol_name(function_name_table[i])) != NULL; i++) {
    o = gc_new_builtin(FUNCITON, symbol_new(s), function_table[i]);
    bind_symbol(o->builtin.name, o);
  }
}

static object parse_args(int argc, char *argv[])
{
  int i;
  object o;
  o = object_nil;
  for (i = 1; i < argc; i++)
    o = gc_new_cons(gc_new_barray_from(STRING, argv[i], strlen(argv[i])), o);
  return o;
}

static void make_initial_objects(int argc, char *argv[])
{
  int i;
  object nil, args, os, home;
  nil = gc_new_barray(SYMBOL, 3);
  memcpy(nil->barray.elt, "nil", 3);
  object_nil = nil;
  object_splay_nil = gc_new_splay_node(nil, nil, nil, nil);
  object_symcmp = gc_new_pointer(&splay_symcmp);
  object_strcmp = gc_new_pointer(&splay_strcmp);
  object_symbol_splay = gc_new_splay(object_strcmp);
  object_keyword_splay = gc_new_splay(object_strcmp);
  splay_add(object_symbol_splay, object_nil, object_nil);
  object_true = symbol_new("true");
  object_key = keyword_new("key");
  object_opt = keyword_new("opt");
  object_rest = keyword_new("rest");
  object_quote = symbol_new("quote");
  object_toplevel = gc_new_env(nil);
  home = symbol_new("$paren-home");
  bind_symbol(home, gc_new_barray_from(STRING, core_fn, strlen(core_fn)));
  object_class = keyword_new("class");
  object_symbol = keyword_new("symbol");
  object_super = keyword_new("super");
  object_features = keyword_new("features");
  object_fields = keyword_new("fields");
  object_message = keyword_new("message");
  object_stack_trace = keyword_new("stack-trace");
  object_Class = symbol_new("Class");
  object_Error = symbol_new("Error");
  args = symbol_new("$args");
  bind_symbol(args, parse_args(argc, argv));
  os = symbol_new("$os");
#if WINDOWS_P
  bind_symbol(os, keyword_new("Windows"));
#elif OS_CODE == OS_LINUX
  bind_symbol(os, keyword_new("Linux"));
#elif OS_CODE == OS_ANDROID
  bind_symbol(os, keyword_new("Android"));
#elif OS_CODE == OS_MACOSX
  bind_symbol(os, keyword_new("Mac"));
#else
  xassert(FALSE);
#endif
  for (i = 0; i < 256; i++) object_bytes[i] = gc_new_bytes(i);
  bind_pseudo_symbol(nil);
  bind_pseudo_symbol(object_true);
  make_builtin();
}

int main(int argc, char *argv[])
{
  int i;
  char buf[MAX_STR_LEN];
  setbuf(stdout, NULL);
  pf_exepath(argv[0], buf);
#if !UNIX_P
  *strrchr(buf, '.') = '\0';
#endif
  strcpy(buf + strlen(buf) - 5, "core.p");
  for (i = 0; i < strlen(buf); i++)
    if (buf[i] == '\\') buf[i] = '/';
  core_fn = buf;
  gc_init();
  make_initial_objects(argc, argv);
  xbarray_init(&token_str);
  object_boot = gc_new_lambda(object_toplevel, object_nil, load());
  ip_start();
  return 0;
}
