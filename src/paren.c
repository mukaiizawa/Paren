// paren main routine.

#include "std.h"
#include "xiconv.h"
#include "pf.h"
#include "object.h"
#include "lex.h"
#include "gc.h"
#include "ip.h"
#include "bi.h"

static char *core_fn;

// parser

static int next_token;
static object parse_expr(void);

static int parse_skip(void)
{
  return next_token = lex();
}

static int parse_token(int token)
{
  char buf[MAX_STR_LEN];
  if (next_token != token) lex_error("missing %s", lex_token_name(buf, token));
  return parse_skip();
}

static object parse_cdr(void)
{
  object o;
  if (next_token == ')') return object_nil;
  o = parse_expr();
  return gc_new_cons(o, parse_cdr());
}

static object parse_list(void)
{
  object o;
  if (parse_skip() == ')') o = object_nil;
  else {
    o = parse_expr();
    o = gc_new_cons(o, parse_cdr());
  }
  parse_token(')');
  return o;
}

static object parse_expr(void)
{
  object o;
  switch (next_token) {
    case '(':
      return parse_list();
    case '\'':
      parse_skip();
      return gc_new_cons(object_quote, gc_new_cons(parse_expr(), object_nil));
    case LEX_SYMBOL:
      o = gc_new_mem_from(SYMBOL, lex_str.elt, lex_str.size);
      parse_skip();
      return o;
    case LEX_KEYWORD:
      o = gc_new_mem_from(KEYWORD, lex_str.elt, lex_str.size);
      parse_skip();
      return o;
    case LEX_STRING:
      o = gc_new_mem_from(STRING, lex_str.elt, lex_str.size);
      parse_skip();
      return o;
    case LEX_INT:
      o = gc_new_xint(lex_ival);
      parse_skip();
      return o;
    case LEX_FLOAT:
      o = gc_new_xfloat(lex_fval);
      parse_skip();
      return o;
    default:
      lex_error("illegal token '%c'", (char)next_token);
      return NULL;
  }
}

// loader

static object load_rec(void)
{
  object o;
  if (next_token == EOF) return object_nil;
  o = parse_expr();
  return gc_new_cons(o, load_rec());
}

static object load(void)
{
  FILE *fp;
  object o;
  if ((fp = fopen(core_fn, "r")) == NULL)
    xerror("load/open %s failed", core_fn);
  lex_start(fp);
  parse_skip();
  o = load_rec();
  fclose(fp);
  return o;
}

static object new_symbol(char *name)
{
  return gc_new_mem_from(SYMBOL, name, strlen(name));
}

static object new_keyword(char *name)
{
  return gc_new_mem_from(KEYWORD, name, strlen(name));
}

static object new_string(char *name)
{
  return gc_new_mem_from(STRING, name, strlen(name));
}

static void make_built_in(void)
{
  int i;
  char buf[MAX_STR_LEN];
  object o;
  for (i = 0; bi_as_symbol_name(special_name_table[i], buf) != NULL; i++) {
    o = gc_new_native(SPECIAL, new_symbol(buf), special_table[i]);
    map_put(object_toplevel, o->native.name, o);
  }
  for (i = 0; bi_as_symbol_name(function_name_table[i], buf) != NULL; i++) {
    o = gc_new_native(BFUNC, new_symbol(buf), function_table[i]);
    map_put(object_toplevel, o->native.name, o);
  }
}

static object parse_args(int argc, char *argv[])
{
  object o;
#if WINDOWS_P
  LPWSTR *wc_args;
  char *mb_arg;
  o = object_nil;
  if ((wc_args = CommandLineToArgvW(GetCommandLineW(), &argc)) == NULL)
    xerror("CommandLineToArgvW/failed");
  while (argc-- > 1) {
    if (!xiconv_wc2mb(XICONV_UTF8, wc_args[argc], &mb_arg))
      xerror("parse_args/failed.");
    o = gc_new_cons(new_string(mb_arg), o);
  }
  LocalFree(wc_args);
#else
  o = object_nil;
  while (argc-- > 1) o = gc_new_cons(new_string(argv[argc]), o);
#endif
  return o;
}

static void make_initial_objects(int argc, char *argv[])
{
  char *host_name;
  object_nil = new_symbol("nil");
  object_true = new_symbol("true");
  object_toplevel = gc_new_env(object_nil, 1 << 11);
  map_put(object_toplevel, object_nil, object_nil);
  map_put(object_toplevel, object_true, object_true);
  object_key = new_keyword("key");
  object_opt = new_keyword("opt");
  object_rest = new_keyword("rest");
  object_quote = new_symbol("quote");
  object_Class = new_symbol("Class");
  object_class = new_keyword("class");
  object_symbol = new_keyword("symbol");
  object_super = new_keyword("super");
  object_features = new_keyword("features");
  object_fields = new_keyword("fields");
  object_Exception = new_symbol("Exception");
  object_message = new_keyword("message");
  object_stack_trace = new_keyword("stack-trace");
  object_status_cd = new_keyword("status-cd");
  map_put(object_toplevel, new_symbol("$args"), parse_args(argc, argv));
  map_put(object_toplevel, new_symbol("core.p"), new_string(core_fn));
#if WINDOWS_P
  host_name = "windows";
#elif OS_CODE == OS_LINUX
  host_name = "linux";
#elif OS_CODE == OS_ANDROID
  host_name = "android";
#elif OS_CODE == OS_MACOSX
  host_name = "mac";
#else
  xassert(FALSE);
#endif
  map_put(object_toplevel, new_symbol("$hostname"), new_keyword(host_name));
  make_built_in();
}

static void init_console()
{
#if WINDOWS_P
  HANDLE h;
  DWORD mode;
  _setmode(_fileno(stdin), _O_BINARY);
  _setmode(_fileno(stdout), _O_BINARY);
  h = GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleMode(h, &mode);
  mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  SetConsoleMode(h, mode);
#endif
  setbuf(stdout, NULL);
}

int main(int argc, char *argv[])
{
  char buf[MAX_STR_LEN];
  init_console();
  xiconv_init();
  pf_exepath(argv[0], buf);
#if !UNIX_P
  *strrchr(buf, '.') = '\0';
#endif
  buf[strlen(buf) - strlen("paren")] = '\0';
  core_fn = strcat(buf, "modules/core.p");
  gc_init();
  make_initial_objects(argc, argv);
  ip_start(load());
  return 0;
}
