// interpreter.

#include "std.h"
#include "om.h"
#include "bi.h"
#include "ip.h"

static long cycle;
static object dr;    // data register.
static object cr;    // context register.
char error_msg[MAX_STR_LEN];

enum TrapType {
  TRAP_NONE,
  TRAP_ERROR,
  TRAP_INTERRUPT
};
static enum TrapType trap_type;

// interrupt

#if UNIX_P

static void intr_handler(int signo)
{
  trap_type = TRAP_INTERRUPT;
}

static void intr_init(void)
{
  struct sigaction sa;
  memset(&sa, 0, sizeof(sa));
  sa.sa_handler = intr_handler;
  if (sigaction(SIGINT, &sa, NULL) == -1) xerror("sigaction/failed");
}

#elif WINDOWS_P

static BOOL intr_handler(DWORD dwCtrlType)
{
  if (dwCtrlType == CTRL_C_EVENT) {
    trap_type = TRAP_INTERRUPT;
    return TRUE;
  }
  return FALSE;
}

static void intr_init(void)
{
  if (SetConsoleCtrlHandler((PHANDLER_ROUTINE)intr_handler, TRUE) == 0)
    xerror("SetConsoleCtrlHandler/failed");
}

#endif

// Exception

static enum Exception e;

static char *error_name(enum Exception e) {
  switch (e) {
    case Exception: return "Exception";
    case SystemExit: return "SystemExit";
    case Error: return "Error";
    case ArgumentError: return "ArgumentError";
    case ArithmeticError: return "ArithmeticError";
    case StateError: return "StateError";
    case OSError: return "OSError";
    case SyntaxError: return "SyntaxError";
    default: xassert(FALSE); return NULL;
  }
};

int ip_sigerr(enum Exception err, char *msg)
{
  e = err;
  trap_type = TRAP_ERROR;
  if (msg == NULL) error_msg[0] = '\0';
  else strcpy(error_msg, msg);
  return FALSE;
}

// stack frame

/*
 *
 * |       ...       |
 * |                 |
 * |                 | <- sp
 * +-----------------+
 * |       ...       |
 * | 3: frame_var[1] |
 * | 2: frame_var[0] |
 * | 1: return addr  |
 * | 0: frame_type   | <- fp
 * +-----------------+
 * |       ...       |
 * | 3: frame_var[1] |
 * | 2: frame_var[0] |
 * | 1: return addr  |
 * | 0: frame_type   |
 * +-----------------+
 * |       ...       |
 * |       ...       |
 * |       ...       |
 * +-----------------+
 * |       ...       |
 * | 3: frame_var[1] |
 * | 2: frame_var[0] |
 * | 1: return addr  |
 * | 0: frame_type   |
 * +-----------------+
 *
 */

#define STACK_GAP 10

#define FRAME_STACK_SIZE 10000
#define FRAME_SIZE_MASK          0x00f
#define   APPLY_BUILT_IN_FRAME   0x003
#define   APPLY_FRAME            0x013
#define   BIND_FRAME             0x023
#define   BIND_HANDLER_FRAME     0x033
#define   BIND_PROPAGATION_FRAME 0x043
#define   BREAK_FRAME            0x052
#define   CATCH_FRAME            0x063
#define   CONTINUE_FRAME         0x072
#define   EVAL_ARGS_FRAME        0x084
#define   EVAL_FRAME             0x092
#define   EVAL_SEQUENTIAL_FRAME  0x0a3
#define   FUNC_FRAME             0x0b4
#define   IF_FRAME               0x0c3
#define   LET_FRAME              0x0d2
#define   LOOP_FRAME             0x0e3
#define   QUOTE_FRAME            0x0f3
#define   RETURN_FRAME           0x102
#define   THROW_FRAME            0x112
#define   UNWIND_PROTECT_FRAME   0x123

#define fs_top() (sint_val(fs[fp]))
#define fs_nth(i) (sint_val(fs[i]))
#define set_fp(i) { fp = i; sp = next_fp(fp); }
#define get_frame_var(base_fp, n) (fs[base_fp + 2 + n])
#define set_frame_var(base_fp, n, o) (fs[base_fp + 2 + n] = o)
#define frame_size(frame_type) (frame_type & FRAME_SIZE_MASK)
#define next_fp(base_fp) (base_fp + frame_size(sint_val(fs[base_fp])))
#define prev_fp(base_fp) (sint_val(fs[base_fp + 1]))
#define gen0(frame_type) gen(frame_type);
#define gen1(frame_type, lv0) \
{ \
  gen0(frame_type); \
  set_frame_var(fp, 0, lv0); \
}
#define gen2(frame_type, lv0, lv1) \
{ \
  gen0(frame_type); \
  set_frame_var(fp, 0, lv0); \
  set_frame_var(fp, 1, lv1); \
}

static int fp;
static int sp;
static object fs[FRAME_STACK_SIZE];

#ifndef NDEBUG
static char *frame_name(int frame_type)
{
  switch (frame_type) {
    case APPLY_BUILT_IN_FRAME: return "APPLY_BUILT_IN_FRAME";
    case APPLY_FRAME: return "APPLY_FRAME";
    case BIND_FRAME: return "BIND_FRAME";
    case BIND_HANDLER_FRAME: return "BIND_HANDLER_FRAME";
    case BIND_PROPAGATION_FRAME: return "BIND_PROPAGATION_FRAME";
    case BREAK_FRAME: return "BREAK_FRAME";
    case CATCH_FRAME: return "CATCH_FRAME";
    case CONTINUE_FRAME: return "CONTINUE_FRAME";
    case EVAL_ARGS_FRAME: return "EVAL_ARGS_FRAME";
    case EVAL_FRAME: return "EVAL_FRAME";
    case EVAL_SEQUENTIAL_FRAME: return "EVAL_SEQUENTIAL_FRAME";
    case FUNC_FRAME: return "FUNC_FRAME";
    case IF_FRAME: return "IF_FRAME";
    case LET_FRAME: return "LET_FRAME";
    case LOOP_FRAME: return "LOOP_FRAME";
    case QUOTE_FRAME: return "QUOTE_FRAME";
    case RETURN_FRAME: return "RETURN_FRAME";
    case THROW_FRAME: return "THROW_FRAME";
    case UNWIND_PROTECT_FRAME: return "UNWIND_PROTECT_FRAME";
    default: xassert(FALSE); return NULL;
  }
}
#endif

#ifndef NDEBUG
static void dump_fs(void)
{
  int i, j, frame_type;
  char buf[MAX_STR_LEN];
  fprintf(stderr, "!stack trace\n");
  for (i = 0; i <= fp; i = next_fp(i)) {
    frame_type = sint_val(fs[i]);
    fprintf(stderr, "+-----------------------------\n");
    fprintf(stderr, "|%d: %s\n", i, frame_name(frame_type));
    fprintf(stderr, "|%d: %d\n", i + 1, sint_val(fs[i + 1]));
    for (j = 0; j < frame_size(frame_type) - 2; j++)
      fprintf(stderr, "|%d: %s\n", i + j + 2, om_describe(get_frame_var(i, j), buf));
  }
  fprintf(stderr, "!registers\n");
  fprintf(stderr, "dr: %s\n", om_describe(dr, buf));
  fprintf(stderr, "cr: %s\n", om_describe(cr, buf));
  exit(1);
}
#endif

static void gen(int frame_type)
{
  if (sp > FRAME_STACK_SIZE - STACK_GAP) ip_sigerr(StateError, "stack over flow");
  fs[sp + 1] = sint(fp);
  fs[sp] = sint(frame_type);
  set_fp(sp);
}

static void pop_frame(void)
{
  sp = fp;
  fp = prev_fp(fp);
}

static void gen_eval_frame(object o)
{
  dr = o;
  switch (om_type(dr)) {
    case SYMBOL:
    case CONS:
      gen0(EVAL_FRAME);
      return;
    default:
      return;
  }
}

static void gen_eval_args_frame(object args)
{
  if (args == om_nil) dr = om_nil;
  else {
    gen2(EVAL_ARGS_FRAME, args->cons.cdr, om_nil);
    gen_eval_frame(args->cons.car);
  }
}

static void gen_eval_sequential_frame(object args)
{
  if (args == om_nil) dr = om_nil;
  else gen1(EVAL_SEQUENTIAL_FRAME, args);
}

static void gen_if_frame(object args)
{
  if (args == om_nil) return;
  if (args->cons.cdr != om_nil) gen1(IF_FRAME, args->cons.cdr);
  gen_eval_frame(args->cons.car);
}

static void gen_trace(object o)
{
  gen2(FUNC_FRAME, cr, o);
}

static int same_symbol_keyword_p(object sym, object key)
{
  xassert(om_type(sym) == SYMBOL);
  xassert(keyword_p(key));
  if (sym->mem.size != key->mem.size - 1) return FALSE;
  return memcmp(sym->mem.elt, key->mem.elt + 1, sym->mem.size) == 0;
}

static int valid_keyword_args(object params, object args)
{
  while (args != om_nil) {
    if (!keyword_p(args->cons.car)) return ip_sigerr(ArgumentError, "expected keyword argument");
    object p = params;
    while (p != om_nil) {
      if (same_symbol_keyword_p(p->cons.car, args->cons.car)) break;
      p = p->cons.cdr;
    }
    if (p == om_nil) return ip_sigerr(ArgumentError, "undeclared keyword parameter");
    if ((args = args->cons.cdr) == om_nil) return ip_sigerr(ArgumentError, "expected keyword parameter value");
    args = args->cons.cdr;
  }
  return TRUE;
}

static int parse_args(void (*f)(object, object, object), object params, object args)
{
  object o, k, v;
  if (!list_p(params) || !list_p(args)) return ip_sigerr(ArgumentError, "expected list");
  // required args
  while (params != om_nil) {
    if (keyword_p(params->cons.car)) break;
    if (args == om_nil) return ip_sigerr(ArgumentError, "too few arguments");
    if (om_type(params->cons.car) == SYMBOL)
      (*f)(cr, params->cons.car, args->cons.car);
    else if (!parse_args(f, params->cons.car, args->cons.car)) return FALSE;
    params = params->cons.cdr;
    args = args->cons.cdr;
  }
  // optional args
  if (params->cons.car == om_opt) {
    params = params->cons.cdr;
    while (params != om_nil) {
      if (keyword_p(params->cons.car)) break;
      k = params->cons.car;
      if (args == om_nil) (*f)(cr, k, om_nil);
      else {
        (*f)(cr, k, args->cons.car);
        args = args->cons.cdr;
      }
      params = params->cons.cdr;
    }
  }
  // rest args
  if (params->cons.car == om_rest) {
    (*f)(cr, params->cons.cdr->cons.car, args);
    return TRUE;
  }
  // keyword args
  else if (params->cons.car == om_key) {
    params = params->cons.cdr;
    if (!valid_keyword_args(params, args)) return FALSE;
    o = args;
    while (params != om_nil) {
      k = params->cons.car;
      v = om_nil;
      args = o;
      while (args != om_nil) {
        if (same_symbol_keyword_p(k, args->cons.car)) {
          v = args->cons.cdr->cons.car;
          break;
        }
        args = args->cons.cdr->cons.cdr;
      }
      (*f)(cr, k, v);
      params = params->cons.cdr;
    }
    return TRUE;
  }
  if (args != om_nil) return ip_sigerr(ArgumentError, "too many arguments");
  return TRUE;
}

static void pop_func_frame(void)
{
  cr = get_frame_var(fp, 0);
  pop_frame();
}

static void pop_let_frame(void)
{
  cr = cr->map.top;
  pop_frame();
}

static void pop_apply_frame(void)
{
  object f, trace;
  f = get_frame_var(fp, 0);
  pop_frame();
  trace = om_new_cons(f, dr);
  // optimize tail recursion
  if (fs_top() == FUNC_FRAME) set_frame_var(fp, 1, trace);
  else gen2(FUNC_FRAME, cr, trace);
  cr = om_new_env(f->proc.env, f->proc.param_count * 2);
  gen_eval_sequential_frame(f->proc.body);
  parse_args(&map_put, f->proc.params, dr);
}

static void pop_apply_built_in_frame(void)
{
  object f, args;
  int (*function)(int, object, object *);
  f = get_frame_var(fp, 0);
  args = dr;
  function = f->native.u.function;
  pop_frame();
  if ((*function)(list_len(args), args, &(dr))) return;
  gen_trace(om_new_cons(f->native.name, args));
  xassert(trap_type != TRAP_NONE);
}

static void pop_bind_frame(void)
{
  object o;
  o = get_frame_var(fp, 0);
  if (om_type(o) == SYMBOL) map_put(cr, o, dr);
  else parse_args(&map_put, o, dr);
  pop_frame();
}

static void pop_bind_handler_frame(void)
{
  object expr, handler;
  expr = get_frame_var(fp, 0);
  pop_frame();
  if (!bi_argv(BI_FUNC, dr, &handler)) {
    gen_trace(om_new_cons(om_new_mem_from(SYMBOL, "catch", 5)
          , om_new_cons(expr
            , om_new_cons(handler, om_nil))));
    return;
  }
  gen1(CATCH_FRAME, handler);
  gen_eval_frame(expr);
}

static void pop_bind_propagation_frame(void)
{
  object o;
  o = get_frame_var(fp, 0);
  if (om_type(o) == SYMBOL) map_put_propagation(cr, o, dr);
  else parse_args(&map_put_propagation, o, dr);
  pop_frame();
}

static int eval_symbol(object *result)
{
  object sym;
  if ((*result = map_get_propagation(cr, (sym = *result))) == NULL) {
    if (keyword_p(sym)) {
      *result = sym;
      return TRUE;
    }
    gen_trace(sym);
    return ip_sigerr(StateError, "unbound symbol");
  }
  return TRUE;
}

static void pop_eval_frame(void)
{
  object args, operator;
  int (*special)(int, object);
  pop_frame();
  switch (om_type(dr)) {
    case SYMBOL:
      eval_symbol(&(dr));
      return;
    case CONS:
      operator = dr->cons.car;
      args = dr->cons.cdr;
      if (om_type(operator) == SYMBOL) {
        if (!eval_symbol(&operator)) return;
      }
      switch (om_type(operator)) {
        case SPECIAL:
          special = operator->native.u.special;
          if ((*special)(list_len(args), args)) return;
          gen_trace(om_new_cons(operator->native.name, args));
          return;
        case BFUNC:
          gen1(APPLY_BUILT_IN_FRAME, operator);
          gen_eval_args_frame(args);
          return;
        case MACRO:
          gen0(EVAL_FRAME);
          gen1(APPLY_FRAME, operator);
          dr = args;
          return;
        case FUNC:
          gen1(APPLY_FRAME, operator);
          gen_eval_args_frame(args);
          return;
        default:
          gen_trace(dr);
          ip_sigerr(StateError, "expected operator");
          return;
      }
      break;
    case SINT:
    case XINT:
    case XFLOAT:
    case STRING:
    case BYTES:
    case ARRAY:
    case DICT:
    case MACRO:
    case FUNC:
    case BFUNC:
    case SPECIAL:
      return;
    default:
      xassert(FALSE);
      return;
  }
}

static void pop_rewinding(void)
{
  switch (fs_top()) {
    case FUNC_FRAME: pop_func_frame(); break;
    case LET_FRAME: pop_let_frame(); break;
    case UNWIND_PROTECT_FRAME: xassert(FALSE); break;    // must be protected.
    default: pop_frame(); break;
  }
}

static void pop_break_continue_frame(int exit_p)
{
  int i;
  object o;
  i = fp;
  while (fp != -1) {
    switch (fs_top()) {
      case LOOP_FRAME:
        dr = om_nil;
        if (exit_p) pop_frame();
        return;
      case UNWIND_PROTECT_FRAME:
        o = get_frame_var(fp, 0);
        pop_frame();
        if (exit_p) gen(BREAK_FRAME);
        else gen(CONTINUE_FRAME);
        gen_eval_sequential_frame(o);
        return;
      default:
        pop_rewinding();
        break;
    }
  }
  set_fp(i);
  ip_sigerr(StateError, "missing loop context");
}

static void pop_unwind_protect_frame(void)
{
  object body;
  body = get_frame_var(fp, 0);
  pop_frame();
  gen1(QUOTE_FRAME, dr);
  gen_eval_sequential_frame(body);
}

static void pop_eval_args_frame(void)
{
  object rest, acc;
  rest = get_frame_var(fp, 0);
  acc = om_new_cons(dr, get_frame_var(fp, 1));
  set_frame_var(fp, 1, acc);
  if (rest == om_nil) {
    pop_frame();
    dr = list_reverse(acc);
  } else {
    set_frame_var(fp, 0, rest->cons.cdr);
    gen_eval_frame(rest->cons.car);
  }
}

static void pop_eval_sequential_frame(void)
{
  object args;
  args = get_frame_var(fp, 0);
  xassert(args != om_nil);
  if (args->cons.cdr == om_nil) pop_frame();
  else set_frame_var(fp, 0, args->cons.cdr);
  gen_eval_frame(args->cons.car);
}

static void pop_if_frame(void)
{
  object args;
  args = get_frame_var(fp, 0);
  pop_frame();
  if (dr != om_nil) gen_eval_frame(args->cons.car);
  else if ((args = args->cons.cdr) != om_nil) gen_if_frame(args);
}

static void pop_return_frame(void)
{
  object o;
  while (fp != -1) {
    switch (fs_top()) {
      case FUNC_FRAME:
        pop_func_frame();
        return;
      case UNWIND_PROTECT_FRAME:
        o = get_frame_var(fp, 0);
        pop_frame();
        gen0(RETURN_FRAME);
        gen1(QUOTE_FRAME, dr);
        gen_eval_sequential_frame(o);
        return;
      default:
        pop_rewinding();
        break;
    }
  }
}

static int pos_is_a_p(object o, object cls_sym);
static object get_call_stack(void);

static void pop_throw_frame(void)
{
  int i;
  object o, handler;
  i = fp;
  pop_frame();
  if (om_type(dr) == DICT && map_get(dr, om_stack_trace) == om_nil)
    map_put(dr, om_stack_trace, get_call_stack());
  while (fp != -1) {
    switch (fs_top()) {
      case UNWIND_PROTECT_FRAME:
        o = get_frame_var(fp, 0);
        pop_frame();
        gen0(THROW_FRAME);
        gen1(QUOTE_FRAME, dr);
        gen_eval_sequential_frame(o);
        return;
      case CATCH_FRAME:
        handler = get_frame_var(fp, 0);
        pop_frame();
        gen1(APPLY_FRAME, handler);
        dr = om_new_cons(dr, om_nil);
        return;
      default:
        pop_rewinding();
        break;
    }
  }
#ifndef NDEBUG
  set_fp(i);
  dump_fs();
#endif
}

// fundamental built in functions

DEFUN(cycle)
{
  dr = om_new_xint(cycle);
  return TRUE;
}

DEFUN(eval)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

DEFUN(apply)
{
  object args;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.cdr->cons.car, &args)) return FALSE;
  dr = args;
  switch (om_type(argv->cons.car)) {
    case BFUNC: gen1(APPLY_BUILT_IN_FRAME, argv->cons.car); return TRUE;
    case FUNC: gen1(APPLY_FRAME, argv->cons.car); return TRUE;
    default: return ip_sigerr(ArgumentError, "expected function");
  }
}

DEFUN(macroexpand_2d_1)
{
  object f, args;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  dr = argv->cons.car;
  if (om_type(dr) != CONS) return TRUE;
  f = dr->cons.car;
  args = dr->cons.cdr;
  if (om_type(f) == SYMBOL) {
    if ((f = map_get_propagation(cr, f)) == NULL) return TRUE;
  }
  if (om_type(f) != MACRO) return TRUE;
  gen1(APPLY_FRAME, f);
  dr = args;
  return TRUE;
}

DEFUN(bound_3f_)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_SYM, argv->cons.car, &o)) return FALSE;
  dr = om_bool(keyword_p(o) || map_get_propagation(cr, o) != NULL);
  return TRUE;
}

// paren object system.

static int pos_om_p(object o)
{
  return om_type(o) == DICT && map_get(o, om_class) != NULL;
}

static int pos_class_p(object o)
{
  object p;
  return om_type(o) == DICT
    && map_get(o, om_class) == om_Class
    && (p = map_get(o, om_symbol)) != NULL
    && om_type(p) == SYMBOL
    && (p = map_get(o, om_super)) != NULL
    && om_type(p) == SYMBOL
    && (p = map_get(o, om_features)) != NULL
    && list_p(p)
    && (p = map_get(o, om_fields)) != NULL
    && list_p(p);
}

static int find_class(object cls_sym, object *result)
{
  if (om_type(cls_sym) != SYMBOL) return FALSE;
  if ((*result = map_get_propagation(cr, cls_sym)) == NULL) return FALSE;
  return pos_class_p(*result);
}

static int find_super_class(object cls_sym, object *result)
{
  object cls;
  if (!find_class(cls_sym, &cls)) return FALSE;
  return find_class(map_get(cls, om_super), result);
}

static int pos_is_a_p(object o, object cls_sym) {
  object o_cls_sym;
  xassert(om_type(cls_sym) == SYMBOL);
  if (!pos_om_p(o)) return FALSE;
  o_cls_sym = map_get(o, om_class);
  while (o_cls_sym != cls_sym) {
    if (!find_super_class(o_cls_sym, &o)) return FALSE;
    o_cls_sym = map_get(o, om_symbol);
  }
  return TRUE;
}

static object find_class_method(object cls_sym, object mtd_sym)
{
  object o;
  struct xbarray buf;
  xassert(om_type(cls_sym) == SYMBOL);
  xassert(om_type(mtd_sym) == SYMBOL);
  xbarray_init(&buf);
  xbarray_copy(&buf, cls_sym->mem.elt, cls_sym->mem.size);
  xbarray_copy(&buf, mtd_sym->mem.elt, mtd_sym->mem.size);
  o = map_get_propagation(cr, om_new_mem_from(SYMBOL, buf.elt, buf.size));
  xbarray_free(&buf);
  return o;
}

DEFUN(is_2d_a_3f_)
{
  object cls;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  dr = om_nil;
  if (!pos_class_p(cls = argv->cons.cdr->cons.car)) return TRUE;
  if (pos_is_a_p(argv->cons.car, map_get(cls, om_symbol))) dr = om_true;
  return TRUE;
}

DEFUN(find_2d_class)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_SYM, argv->cons.car, &o)) return FALSE;
  if (!find_class(o, result)) *result = om_nil;
  return TRUE;
}

DEFUN(find_2d_method)
{
  object cls, cls_sym, mtd_sym, features;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_argv(BI_SYM, argv->cons.car, &cls_sym)) return FALSE;
  if (!bi_argv(BI_SYM, argv->cons.cdr->cons.car, &mtd_sym)) return FALSE;
  while (TRUE) {
    // class method
    if ((*result = find_class_method(cls_sym, mtd_sym)) != NULL) return TRUE;
    // feature method
    if (!find_class(cls_sym, &cls)) return ip_sigerr(ArgumentError, "undeclared class");
    features = map_get(cls, om_features);
    while (features != om_nil) {
      if ((*result = find_class_method(features->cons.car, mtd_sym)) != NULL) return TRUE;
      features = features->cons.cdr;
    }
    // super class method
    if (!find_super_class(cls_sym, &cls)) return ip_sigerr(StateError, "undeclared super class");
    cls_sym = map_get(cls, om_symbol);
  }
}

// special operator

static int param_count;

static int parse_rest_param(object params)
{
  if (om_type(params->cons.car) == SYMBOL && params->cons.cdr == om_nil) {
    param_count++;
    return TRUE;
  }
  return ip_sigerr(SyntaxError, "only one symbol can be specified for rest parameter");
}

static int parse_keyword_params(object params)
{
  while (params != om_nil) {
    object p = params->cons.car;
    switch (om_type(p)) {
      case SYMBOL:
        if (p == om_opt) return ip_sigerr(SyntaxError, "optional parameter cannot be specified after keyword parameter");
        if (p == om_rest) return ip_sigerr(SyntaxError, "keyword parmeter and rest parameter cannot be mixed");
        param_count++;
        break;
      default:
        return ip_sigerr(SyntaxError, "expected symbol for keyword parameter");
    }
    params = params->cons.cdr;
  }
  return TRUE;
}

static int parse_optional_params(object params)
{
  while (params != om_nil) {
    object p = params->cons.car;
    switch (om_type(p)) {
      case SYMBOL:
        if (p == om_key) return parse_keyword_params(params->cons.cdr);
        if (p == om_rest) return parse_rest_param(params->cons.cdr);
        param_count++;
        break;
      default:
        return ip_sigerr(SyntaxError, "expected symbol for optional parameter");
    }
    params = params->cons.cdr;
  }
  return TRUE;
}

static int parse_required_params(object params)
{
  while (params != om_nil) {
    object p = params->cons.car;
    switch (om_type(p)) {
      case CONS:
        if (!parse_required_params(p)) return FALSE;
        break;
      case SYMBOL:
        if (p == om_opt) return parse_optional_params(params->cons.cdr);
        if (p == om_key) return parse_keyword_params(params->cons.cdr);
        if (p == om_rest) return parse_rest_param(params->cons.cdr);
        param_count++;
        break;
      default:
        return ip_sigerr(SyntaxError, "expected symbol or list in binding expression");
    }
    params = params->cons.cdr;
  }
  return TRUE;
}

static int parse_params(object params)
{
  param_count = 0;
  return parse_required_params(params);
}

static int gen_bind_frame(int frame_type, object args)
{
  object o;
  if ((o = args) == om_nil) return TRUE;
  switch (om_type(o->cons.car)) {
    case SYMBOL:
      param_count++;
      break;
    case CONS:
      if (!parse_required_params(o->cons.car)) return FALSE;
      break;
    default:
      return ip_sigerr(SyntaxError, "expected symbol or list in binding expression");
  }
  if ((args = args->cons.cdr) == om_nil) return ip_sigerr(ArgumentError, "missing binding value");
  if (!gen_bind_frame(frame_type, args->cons.cdr)) return FALSE;
  gen1(frame_type, o->cons.car);
  gen0(EVAL_FRAME);
  gen1(QUOTE_FRAME, o->cons.cdr->cons.car);
  return TRUE;
}

static int gen_bind_frames(int frame_type, object args)
{
  param_count = 0;
  return gen_bind_frame(frame_type, args);
}

DEFSP(let)
{
  object binds;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.car, &binds)) return FALSE;
  if (binds == om_nil) gen_eval_sequential_frame(argv->cons.cdr);
  else {
    gen0(LET_FRAME);
    gen_eval_sequential_frame(argv->cons.cdr);
    if (!gen_bind_frames(BIND_FRAME, binds)) return FALSE;
    cr = om_new_env(cr, param_count * 2);
  }
  return TRUE;
}

DEFSP(dynamic)
{
  int i;
  object e, s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_SYM, argv->cons.car, &s)) return FALSE;
  i = fp;
  e = cr;
  if ((dr = map_get(e, s)) != NULL) return TRUE;
  while ((i = prev_fp(i)) != -1) {
    switch (fs_nth(i)) {
      case LET_FRAME: e = e->map.top; break;
      case FUNC_FRAME: e = get_frame_var(i, 0); break;
      default: continue;
    }
    if ((dr = map_get(e, s)) != NULL) return TRUE;
  }
  dr = om_nil;
  return ip_sigerr(ArgumentError, "unbound symbol");
}

DEFSP(_3c__2d_)
{
  return gen_bind_frames(BIND_PROPAGATION_FRAME, argv);
}

DEFSP(begin)
{
  gen_eval_sequential_frame(argv);
  return TRUE;
}

DEFSP(macro)
{
  object o, params;
  if (!bi_argc_range(argc, 3, FALSE)) return FALSE;
  if (!bi_argv(BI_SYM, argv->cons.car, &o)) return FALSE;
  gen1(BIND_PROPAGATION_FRAME, o);
  argv = argv->cons.cdr;
  if (!bi_argv(BI_LIST, argv->cons.car, &params)) return FALSE;
  if (!parse_params(params)) return FALSE;
  dr = om_new_macro(cr, param_count, params, argv->cons.cdr);
  return TRUE;
}

DEFSP(f)
{
  object params;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.car, &params)) return FALSE;
  if (!parse_params(params)) return FALSE;
  dr = om_new_func(cr, param_count, params, argv->cons.cdr);
  return TRUE;
}

DEFSP(quote)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  dr = argv->cons.car;
  return TRUE;
}

DEFSP(if)
{
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  gen_if_frame(argv);
  return TRUE;
}

DEFSP(unwind_2d_protect)
{
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  gen1(UNWIND_PROTECT_FRAME, argv->cons.cdr);
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

DEFSP(break)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  gen0(BREAK_FRAME);
  return TRUE;
}

DEFSP(continue)
{
  if (!bi_argc_range(argc, FALSE, FALSE)) return FALSE;
  gen0(CONTINUE_FRAME);
  return TRUE;
}

DEFSP(loop)
{
  gen1(LOOP_FRAME, argv);
  gen_eval_sequential_frame(argv);
  return TRUE;
}

DEFSP(throw)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  gen0(THROW_FRAME);
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

DEFSP(catch)
{
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  gen1(BIND_HANDLER_FRAME, argv->cons.car);
  gen_eval_frame(argv->cons.cdr->cons.car);
  return TRUE;
}

DEFSP(return)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  gen0(RETURN_FRAME);
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

static object anonimous_proc;
static object named_proc;

static void find_named_proc(void *key, void *data)
{
  if (anonimous_proc == data) named_proc = key;
}

static int resolve_anonimous_proc(void)
{
  object e;
  xassert(named_proc == NULL);
  e = cr;
  while (e != om_nil) {
    map_foreach(e, find_named_proc);
    if (named_proc != NULL) return TRUE;
    e = e->map.top;
  }
  return FALSE;
}

static object get_call_stack(void)
{
  int i;
  object o, p, f, args;
  o = om_nil;
  for (i = 0; i <= fp; i = next_fp(i)) {
    switch (fs_nth(i)) {
      case FUNC_FRAME:
        p = get_frame_var(i, 1);
        if (om_type(p) != CONS) o = om_new_cons(p, o);
        else {
          f = p->cons.car;
          args = p->cons.cdr;
          named_proc = NULL;
          anonimous_proc = f;
          if (resolve_anonimous_proc()) f = named_proc;
          else f = anonimous_proc;
          o = om_new_cons(om_new_cons(f, args), o);
        }
        break;
      default:
        break;
    }
  }
  return o;
}

static object new_Error(enum Exception e, object message)
{
  object o;
  o = om_new_dict();
  map_put(o, om_class, om_new_mem_from_cstr(SYMBOL, error_name(e)));
  map_put(o, om_message, message);
  map_put(o, om_stack_trace, om_nil);
  return o;
}

static void trap(void)
{
  gen0(THROW_FRAME);
  switch (trap_type) {
    case TRAP_ERROR:
      dr = new_Error(e, om_new_mem_from_cstr(STRING, error_msg));
      break;
    case TRAP_INTERRUPT:
      dr = new_Error(SystemExit, om_nil);
      break;
    default:
      xassert(FALSE);
      break;
  }
  trap_type = TRAP_NONE;
}

// main

static void ip_main(object args)
{
  dr = om_nil;
  cr = om_toplevel;
  gen_eval_sequential_frame(args);
  while (fp != -1) {
    xassert(fp >= 0);
    if (trap_type != TRAP_NONE) trap();
    if (cycle % IP_POLLING_INTERVAL == 0) om_gc_chance();
    switch (fs_top()) {
      case APPLY_BUILT_IN_FRAME: pop_apply_built_in_frame(); break;
      case APPLY_FRAME: pop_apply_frame(); break;
      case BIND_FRAME: pop_bind_frame(); break;
      case BIND_HANDLER_FRAME: pop_bind_handler_frame(); break;
      case BIND_PROPAGATION_FRAME: pop_bind_propagation_frame(); break;
      case BREAK_FRAME: pop_break_continue_frame(TRUE); break;
      case CONTINUE_FRAME: pop_break_continue_frame(FALSE); break;
      case EVAL_ARGS_FRAME: pop_eval_args_frame(); break;
      case EVAL_FRAME: pop_eval_frame(); break;
      case EVAL_SEQUENTIAL_FRAME: pop_eval_sequential_frame(); break;
      case FUNC_FRAME: pop_func_frame(); break;
      case CATCH_FRAME: pop_frame(); break;
      case IF_FRAME: pop_if_frame(); break;
      case LET_FRAME: pop_let_frame(); break;
      case LOOP_FRAME: gen_eval_sequential_frame(get_frame_var(fp, 0)); break;
      case QUOTE_FRAME: dr = get_frame_var(fp, 0); pop_frame(); break;
      case RETURN_FRAME: pop_return_frame(); break;
      case THROW_FRAME: pop_throw_frame(); break;
      case UNWIND_PROTECT_FRAME: pop_unwind_protect_frame(); break;
      default: xassert(FALSE); break;
    }
    cycle++;
  }
}

void ip_mark_object(void)
{
  int i;
  om_mark(dr);
  om_mark(cr);
  om_mark(om_toplevel);
  for (i = 0; i < sp; i++) om_mark(fs[i]);
  om_mark(om_toplevel);
  om_mark(om_nil);
  om_mark(om_true);
  om_mark(om_key);
  om_mark(om_opt);
  om_mark(om_rest);
  om_mark(om_quote);
  om_mark(om_stack_trace);
  om_mark(om_Class);
  om_mark(om_Exception);
  om_mark(om_class);
  om_mark(om_symbol);
  om_mark(om_super);
  om_mark(om_features);
  om_mark(om_fields);
  om_mark(om_message);
}

int ip_start(object args)
{
  sp = 0;
  fp = -1;
  cycle = 0;
  trap_type = TRAP_NONE;
  intr_init();
  ip_main(args);
  if (sint_p(dr)) return sint_val(dr);
  return 1;
}
