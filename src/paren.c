// paren main routine.

#include "std.h"
#include "xiconv.h"
#include "pf.h"
#include "om.h"
#include "lex.h"
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
  if (next_token == ')') return om_nil;
  o = parse_expr();
  return om_new_cons(o, parse_cdr());
}

static object parse_list(void)
{
  object o;
  if (parse_skip() == ')') o = om_nil;
  else {
    o = parse_expr();
    o = om_new_cons(o, parse_cdr());
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
      return om_new_cons(om_quote, om_new_cons(parse_expr(), om_nil));
    case LEX_SYMBOL:
      o = om_new_mem_from(SYMBOL, lex_str.elt, lex_str.size);
      parse_skip();
      return o;
    case LEX_STRING:
      o = om_new_mem_from(STRING, lex_str.elt, lex_str.size);
      parse_skip();
      return o;
    case LEX_INT:
      o = om_new_xint(lex_ival);
      parse_skip();
      return o;
    case LEX_FLOAT:
      o = om_new_xfloat(lex_fval);
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
  if (next_token == EOF) return om_nil;
  o = parse_expr();
  return om_new_cons(o, load_rec());
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

static int digit_val(char ch)
{
  if (isdigit(ch)) return ch - '0';
  return ch - 'a' + 10;
}

static char *built_in_name(char *name, char *buf)
{
  char len;
  int s, t;
  if (name == NULL) return NULL;
  s = t = 0;
  len = strlen(name);
  while (s < len) {
    if (name[s] != '_') buf[t++] = name[s++];
    else {
      buf[t++] = 16 * digit_val(name[s + 1]) + digit_val(name[s + 2]);
      s += 4;
    }
  }
  buf[t] = '\0';
  return buf;
}

static void make_built_in(void)
{
  int i;
  char buf[MAX_STR_LEN];
  object o;
  for (i = 0; built_in_name(special_name_table[i], buf) != NULL; i++) {
    o = om_new_native(SPECIAL, om_new_mem_from_cstr(SYMBOL, buf), special_table[i]);
    map_put(om_toplevel, o->native.name, o);
  }
  for (i = 0; built_in_name(function_name_table[i], buf) != NULL; i++) {
    o = om_new_native(BFUNC, om_new_mem_from_cstr(SYMBOL, buf), function_table[i]);
    map_put(om_toplevel, o->native.name, o);
  }
}

static object parse_args(int argc, char *argv[])
{
  object o;
#if WINDOWS_P
  LPWSTR *wc_args;
  char *mb_arg;
  o = om_nil;
  if ((wc_args = CommandLineToArgvW(GetCommandLineW(), &argc)) == NULL)
    xerror("CommandLineToArgvW/failed");
  while (argc-- > 1) {
    if (!xiconv_wc2mb(XICONV_UTF8, wc_args[argc], &mb_arg))
      xerror("parse_args/failed.");
    o = om_new_cons(om_new_mem_from_cstr(STRING, mb_arg), o);
  }
  LocalFree(wc_args);
#else
  o = om_nil;
  while (argc-- > 1) o = om_new_cons(om_new_mem_from_cstr(STRING, argv[argc]), o);
#endif
  return o;
}

static void make_initial_objects(int argc, char *argv[])
{
  char *host_name;
  om_nil = om_new_mem_from_cstr(SYMBOL, "nil");
  om_true = om_new_mem_from_cstr(SYMBOL, "true");
  om_toplevel = om_new_env(om_nil, 1 << 11);
  map_put(om_toplevel, om_nil, om_nil);
  map_put(om_toplevel, om_true, om_true);
  om_key = om_new_mem_from_cstr(SYMBOL, ":key");
  om_opt = om_new_mem_from_cstr(SYMBOL, ":opt");
  om_rest = om_new_mem_from_cstr(SYMBOL, ":rest");
  om_quote = om_new_mem_from_cstr(SYMBOL, "quote");
  om_Class = om_new_mem_from_cstr(SYMBOL, "Class");
  om_class = om_new_mem_from_cstr(SYMBOL, ":class");
  om_symbol = om_new_mem_from_cstr(SYMBOL, ":symbol");
  om_super = om_new_mem_from_cstr(SYMBOL, ":super");
  om_features = om_new_mem_from_cstr(SYMBOL, ":features");
  om_fields = om_new_mem_from_cstr(SYMBOL, ":fields");
  om_Exception = om_new_mem_from_cstr(SYMBOL, "Exception");
  om_message = om_new_mem_from_cstr(SYMBOL, ":message");
  om_stack_trace = om_new_mem_from_cstr(SYMBOL, ":stack-trace");
  map_put(om_toplevel, om_new_mem_from_cstr(SYMBOL, "$args"), parse_args(argc, argv));
  map_put(om_toplevel, om_new_mem_from_cstr(SYMBOL, "core.p"), om_new_mem_from_cstr(STRING, core_fn));
#if WINDOWS_P
  host_name = ":windows";
#elif OS_CODE == OS_LINUX
  host_name = ":linux";
#elif OS_CODE == OS_ANDROID
  host_name = ":android";
#elif OS_CODE == OS_MACOSX
  host_name = ":mac";
#else
  xassert(FALSE);
#endif
  map_put(om_toplevel, om_new_mem_from_cstr(SYMBOL, "$hostname"), om_new_mem_from_cstr(SYMBOL, host_name));
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
  om_init();
  make_initial_objects(argc, argv);
  return ip_start(load());
}
