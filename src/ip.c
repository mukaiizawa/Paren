// interpreter.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static long cycle;

static object dr;    // data register.
static object cr;    // context register.

// interrupt

int ip_trap_code;

#if UNIX_P

static void intr_handler(int signo)
{
  ip_trap_code = TRAP_INTERRUPT;
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
    ip_trap_code = TRAP_INTERRUPT;
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

// error

static enum error e;
static enum error_msg em;

static char *error_name(enum error e) {
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

static char *error_msg(enum error_msg em) {
  switch (em) {
    case bi_buf_msg: return bi_buf.elt;
    case built_in_failed: return "built-in function failed";
    case clip_failed: return "clip failed";
    case connection_failed: return "connection failed";
    case division_by_zero: return "division by zero";
    case error_msg_nil: return NULL;
    case expected_binding_value: return "expected binding value";
    case expected_built_in_operator: return "expected built-in operator";
    case expected_byte: return "expected byte";
    case expected_function: return "expected function";
    case expected_integer: return "expected integer";
    case expected_keyword_parameter_value: return "expected keyword parameter value";
    case expected_list: return "expected list";
    case expected_loop_context: return "expected loop context";
    case expected_number: return "expected number";
    case expected_operator: return "expected operator";
    case expected_positive_integer: return "expected positive integer";
    case expected_positive_integer_or_sequence: return "expected positive integer, array, bytes, list, or string";
    case fgetc_failed: return "fgetc failed";
    case fopen_failed: return "fopen failed";
    case fp_failed: return "fp failed";
    case fputc_failed: return "fputc failed";
    case fread_failed: return "fread failed";
    case fseek_failed: return "fseek failed";
    case ftell_failed: return "ftell failed";
    case fwrite_failed: return "fwrite failed";
    case getaddrinfo_failed: return "getaddrinfo failed";
    case gethostname_failed: return "gethostname failed";
    case incomplete_utf8_byte_sequence: return "incomplete utf8 byte sequence";
    case index_out_of_range: return "index out of range";
    case invalid_args: return "invalid arguments";
    case invalid_binding_expr: return "invalid binding expression";
    case numeric_overflow: return "numeric overflow";
    case readdir_failed: return "readdir failed";
    case recv_failed: return "recv failed";
    case send_failed: return "send failed";
    case socket_startup_failed: return "socket startup failed";
    case stack_over_flow: return "stack over flow";
    case stat_failed: return "stat failed";
    case too_few_arguments: return "too few arguments";
    case too_many_arguments: return "too many arguments";
    case unbound_symbol: return "unbound symbol";
    case undeclared_class: return "undeclared class";
    case undeclared_keyword_param: return "undeclared keyword parameter";
    case unexpected_keyword_parameter: return "unexpected keyword parameter";
    case unknown_af_family: return "unknown address family";
    case unknown_socktype: return "unknown socket type";
    case unexpected_utf8_leading_byte: return "unexpected utf8 leading byte";
    default: xassert(FALSE); return NULL;
  }
};

int ip_throw(enum error err, enum error_msg msg)
{
  ip_trap_code = TRAP_ERROR;
  e = err;
  em = msg;
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
  for (i = 0; i <= fp; i = next_fp(i)) {
    frame_type = sint_val(fs[i]);
    fprintf(stderr, "+-----------------------------\n");
    fprintf(stderr, "|%d: %s\n", i, frame_name(frame_type));
    fprintf(stderr, "|%d: %d\n", i + 1, sint_val(fs[i + 1]));
    for (j = 0; j < frame_size(frame_type) - 2; j++)
      fprintf(stderr, "|%d: %s\n", i + j + 2, object_describe(get_frame_var(i, j), buf));
  }
  fprintf(stderr, "dr: %s", object_describe(dr, buf));
  fprintf(stderr, "cr: %s", object_describe(cr, buf));
  xerror("illegal state");
}
#endif

static void gen(int frame_type)
{
  if (sp > FRAME_STACK_SIZE - STACK_GAP) ip_throw(StateError, stack_over_flow);
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
  switch (object_type(dr)) {
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
  if (args == object_nil) dr = object_nil;
  else {
    gen2(EVAL_ARGS_FRAME, args->cons.cdr, object_nil);
    gen_eval_frame(args->cons.car);
  }
}

static void gen_eval_sequential_frame(object args)
{
  if (args == object_nil) dr = object_nil;
  else gen1(EVAL_SEQUENTIAL_FRAME, args);
}

static void gen_if_frame(object args)
{
  if (args == object_nil) return;
  if (args->cons.cdr != object_nil) gen1(IF_FRAME, args->cons.cdr);
  gen_eval_frame(args->cons.car);
}

static void gen_trace(object o)
{
  gen2(FUNC_FRAME, cr, o);
}

static int same_symbol_keyword_p(object sym, object key)
{
  xassert(object_type(sym) == SYMBOL);
  xassert(object_type(key) == KEYWORD);
  if (sym->mem.size != key->mem.size) return FALSE;
  return memcmp(sym->mem.elt, key->mem.elt, sym->mem.size) == 0;
}

static int valid_keyword_args(object params, object args)
{
  object p;
  while (args != object_nil) {
    if (object_type(args->cons.car) != KEYWORD)
      return ip_throw(ArgumentError, error_msg_nil);
    p = params;
    while (p != object_nil) {
      if (same_symbol_keyword_p(p->cons.car, args->cons.car)) break;
      p = p->cons.cdr;
    }
    if (p == object_nil)
      return ip_throw(ArgumentError, undeclared_keyword_param);
    if ((args = args->cons.cdr) == object_nil)
      return ip_throw(ArgumentError, expected_keyword_parameter_value);
    args = args->cons.cdr;
  }
  return TRUE;
}

static int parse_args(void (*f)(object, object, object), object params, object args)
{
  object o, k, v;
  if (!list_p(params) || !list_p(args))
    return ip_throw(ArgumentError, expected_list);
  // required args
  while (params != object_nil) {
    if (object_type(params->cons.car) == KEYWORD) break;
    if (args == object_nil)
      return ip_throw(ArgumentError, too_few_arguments);
    if (object_type(params->cons.car) == SYMBOL)
      (*f)(cr, params->cons.car, args->cons.car);
    else if (!parse_args(f, params->cons.car, args->cons.car)) return FALSE;
    params = params->cons.cdr;
    args = args->cons.cdr;
  }
  // optional args
  if (params->cons.car == object_opt) {
    params = params->cons.cdr;
    while (params != object_nil) {
      if (object_type(params->cons.car) == KEYWORD) break;
      k = params->cons.car;
      if (args == object_nil) (*f)(cr, k, object_nil);
      else {
        (*f)(cr, k, args->cons.car);
        args = args->cons.cdr;
      }
      params = params->cons.cdr;
    }
  }
  // rest args
  if (params->cons.car == object_rest) {
    (*f)(cr, params->cons.cdr->cons.car, args);
    return TRUE;
  }
  // keyword args
  else if (params->cons.car == object_key) {
    params = params->cons.cdr;
    if (!valid_keyword_args(params, args)) return FALSE;
    o = args;
    while (params != object_nil) {
      k = params->cons.car;
      v = object_nil;
      args = o;
      while (args != object_nil) {
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
  if (args != object_nil)
    return ip_throw(ArgumentError, too_many_arguments);
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
  trace = gc_new_cons(f, dr);
  // optimize tail recursion
  if (fs_top() == FUNC_FRAME) set_frame_var(fp, 1, trace);
  else gen2(FUNC_FRAME, cr, trace);
  cr = gc_new_env(f->proc.env, f->proc.param_count * 2);
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
  gen_trace(gc_new_cons(f->native.name, args));
  if (ip_trap_code == TRAP_NONE) ip_throw(Error, built_in_failed);
}

static void pop_bind_frame(void)
{
  object o;
  o = get_frame_var(fp, 0);
  if (object_type(o) == SYMBOL) map_put(cr, o, dr);
  else parse_args(&map_put, o, dr);
  pop_frame();
}

static void pop_bind_handler_frame(void)
{
  object expr, handler;
  expr = get_frame_var(fp, 0);
  pop_frame();
  if (!bi_argv(BI_FUNC, dr, &handler)) {
    gen_trace(gc_new_cons(gc_new_mem_from(SYMBOL, "catch", 5)
          , gc_new_cons(expr
            , gc_new_cons(handler, object_nil))));
    return;
  }
  gen1(CATCH_FRAME, handler);
  gen_eval_frame(expr);
}

static void pop_bind_propagation_frame(void)
{
  object o;
  o = get_frame_var(fp, 0);
  if (object_type(o) == SYMBOL) map_put_propagation(cr, o, dr);
  else parse_args(&map_put_propagation, o, dr);
  pop_frame();
}

static int eval_symbol(object *result)
{
  object o;
  o = *result;
  if ((*result = map_get_propagation(cr, *result)) == NULL) {
    gen_trace(o);
    return ip_throw(StateError, unbound_symbol);
  }
  return TRUE;
}

static void pop_eval_frame(void)
{
  object args, operator;
  int (*special)(int, object);
  pop_frame();
  switch (object_type(dr)) {
    case SYMBOL:
      eval_symbol(&(dr));
      return;
    case CONS:
      operator = dr->cons.car;
      args = dr->cons.cdr;
      if (object_type(operator) == SYMBOL) {
        if (!eval_symbol(&operator)) return;
      }
      switch (object_type(operator)) {
        case SPECIAL:
          special = operator->native.u.special;
          if ((*special)(list_len(args), args)) return;
          gen_trace(gc_new_cons(operator->native.name, args));
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
          ip_throw(StateError, expected_operator);
          return;
      }
      break;
    case SINT:
    case XINT:
    case XFLOAT:
    case KEYWORD:
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
        dr = object_nil;
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
  ip_throw(StateError, expected_loop_context);
  set_fp(i);
  return;
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
  acc = gc_new_cons(dr, get_frame_var(fp, 1));
  set_frame_var(fp, 1, acc);
  if (rest == object_nil) {
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
  xassert(args != object_nil);
  if (args->cons.cdr == object_nil) pop_frame();
  else set_frame_var(fp, 0, args->cons.cdr);
  gen_eval_frame(args->cons.car);
}

static void pop_if_frame(void)
{
  object args;
  args = get_frame_var(fp, 0);
  pop_frame();
  if (dr != object_nil) gen_eval_frame(args->cons.car);
  else if ((args = args->cons.cdr) != object_nil) gen_if_frame(args);
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
  if (object_type(dr) == DICT && map_get(dr, object_stack_trace) == object_nil)
    map_put(dr, object_stack_trace, get_call_stack());
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
        dr = gc_new_cons(dr, object_nil);
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
  dr = gc_new_xint(cycle);
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
  switch (object_type(argv->cons.car)) {
    case BFUNC:
      gen1(APPLY_BUILT_IN_FRAME, argv->cons.car);
      return TRUE;
    case FUNC:
      gen1(APPLY_FRAME, argv->cons.car);
      return TRUE;
    default:
      return ip_throw(ArgumentError, expected_function);
  }
}

DEFUN(macroexpand_2d_1)
{
  object f, args;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  dr = argv->cons.car;
  if (object_type(dr) != CONS) return TRUE;
  f = dr->cons.car;
  args = dr->cons.cdr;
  if (object_type(f) == SYMBOL) {
    if ((f = map_get_propagation(cr, f)) == NULL) return TRUE;
  }
  if (object_type(f) != MACRO) return TRUE;
  gen1(APPLY_FRAME, f);
  dr = args;
  return TRUE;
}

DEFUN(bound_3f_)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_SYM, argv->cons.car, &o)) return FALSE;
  dr = object_bool(map_get_propagation(cr, o) != NULL);
  return TRUE;
}

// paren object system.

static int pos_object_p(object o)
{
  return object_type(o) == DICT && map_get(o, object_class) != NULL;
}

static int pos_class_p(object o)
{
  object p;
  return object_type(o) == DICT
    && map_get(o, object_class) == object_Class
    && (p = map_get(o, object_symbol)) != NULL
    && object_type(p) == SYMBOL
    && (p = map_get(o, object_super)) != NULL
    && object_type(p) == SYMBOL
    && (p = map_get(o, object_features)) != NULL
    && list_p(p)
    && (p = map_get(o, object_fields)) != NULL
    && list_p(p);
}

static int find_class(object cls_sym, object *result)
{
  if (object_type(cls_sym) != SYMBOL) return FALSE;
  if ((*result = map_get_propagation(cr, cls_sym)) == NULL) return FALSE;
  return pos_class_p(*result);
}

static int find_super_class(object cls_sym, object *result)
{
  object cls;
  if (!find_class(cls_sym, &cls)) return FALSE;
  return find_class(map_get(cls, object_super), result);
}


static int pos_is_a_p(object o, object cls_sym) {
  object o_cls_sym;
  xassert(object_type(cls_sym) == SYMBOL);
  if (!pos_object_p(o)) return FALSE;
  o_cls_sym = map_get(o, object_class);
  while (o_cls_sym != cls_sym) {
    if (!find_super_class(o_cls_sym, &o)) return FALSE;
    o_cls_sym = map_get(o, object_symbol);
  }
  return TRUE;
}

static object find_class_method(object cls_sym, object mtd_sym)
{
  xassert(object_type(cls_sym) == SYMBOL);
  xassert(object_type(mtd_sym) == SYMBOL);
  xbarray_reset(&bi_buf);
  xbarray_copy(&bi_buf, cls_sym->mem.elt, cls_sym->mem.size);
  xbarray_copy(&bi_buf, mtd_sym->mem.elt, mtd_sym->mem.size);
  return map_get_propagation(cr, gc_new_mem_from(SYMBOL, bi_buf.elt, bi_buf.size));
}

DEFUN(is_2d_a_3f_)
{
  object cls;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  dr = object_nil;
  if (!pos_class_p(cls = argv->cons.cdr->cons.car)) return TRUE;
  if (pos_is_a_p(argv->cons.car, map_get(cls, object_symbol))) dr = object_true;
  return TRUE;
}

DEFUN(find_2d_class)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_argv(BI_SYM, argv->cons.car, &o)) return FALSE;
  if (!find_class(o, result)) *result = object_nil;
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
    if (!find_class(cls_sym, &cls)) return ip_throw(ArgumentError, undeclared_class);
    features = map_get(cls, object_features);
    while (features != object_nil) {
      if ((*result = find_class_method(features->cons.car, mtd_sym)) != NULL) return TRUE;
      features = features->cons.cdr;
    }
    // super class method
    if (!find_super_class(cls_sym, &cls))
      return ip_throw(StateError, unbound_symbol);
    cls_sym = map_get(cls, object_symbol);
  }
}

// special operator

static int param_count;

static int parse_rest_param(object params)
{
  switch (object_type(params->cons.car)) {
    case SYMBOL:
      param_count++;
      if (params->cons.cdr == object_nil) return TRUE;
      break;
    default:
      break;
  }
  return ip_throw(SyntaxError, invalid_binding_expr);
}

static int parse_keyword_params(object params)
{
  while (params != object_nil) {
    switch (object_type(params->cons.car)) {
      case SYMBOL:
        params = params->cons.cdr;
        param_count++;
        break;
      default:
        return ip_throw(SyntaxError, invalid_binding_expr);
    }
  }
  return TRUE;
}

static int parse_optional_params(object params)
{
  object p;
  while (params != object_nil) {
    p = params->cons.car;
    switch (object_type(params->cons.car)) {
      case KEYWORD:
        if (p == object_key) return parse_keyword_params(params->cons.cdr);
        if (p == object_rest) return parse_rest_param(params->cons.cdr);
        return ip_throw(SyntaxError, unexpected_keyword_parameter);
      case SYMBOL:
        params = params->cons.cdr;
        param_count++;
        break;
      default:
        return ip_throw(SyntaxError, invalid_binding_expr);
    }
  }
  return TRUE;
}

static int parse_required_params(object params)
{
  object p;
  while (params != object_nil) {
    p = params->cons.car;
    switch (object_type(params->cons.car)) {
      case KEYWORD:
        if (p == object_opt) return parse_optional_params(params->cons.cdr);
        if (p == object_key) return parse_keyword_params(params->cons.cdr);
        if (p == object_rest) return parse_rest_param(params->cons.cdr);
        return ip_throw(SyntaxError, unexpected_keyword_parameter);
      case CONS:
        if (!parse_required_params(params->cons.car)) return FALSE;
        params = params->cons.cdr;
        break;
      case SYMBOL:
        params = params->cons.cdr;
        param_count++;
        break;
      default:
        return ip_throw(SyntaxError, invalid_binding_expr);
    }
  }
  return TRUE;
}

static int gen_bind_frames(int frame_type, object args)
{
  object o;
  if ((o = args) == object_nil) return TRUE;
  switch (object_type(o->cons.car)) {
    case SYMBOL:
      param_count++;
      break;
    case CONS:
      if (!parse_required_params(o->cons.car)) return FALSE;
      break;
    default:
      return ip_throw(SyntaxError, invalid_binding_expr);
  }
  if ((args = args->cons.cdr) == object_nil)
    return ip_throw(ArgumentError, expected_binding_value);
  if (!gen_bind_frames(frame_type, args->cons.cdr)) return FALSE;
  gen1(frame_type, o->cons.car);
  gen0(EVAL_FRAME);
  gen1(QUOTE_FRAME, o->cons.cdr->cons.car);
  return TRUE;
}

DEFSP(let)
{
  object binds;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.car, &binds)) return FALSE;
  if (binds == object_nil) gen_eval_sequential_frame(argv->cons.cdr);
  else {
    gen0(LET_FRAME);
    gen_eval_sequential_frame(argv->cons.cdr);
    param_count = 0;
    if (!gen_bind_frames(BIND_FRAME, binds)) return FALSE;
    cr = gc_new_env(cr, param_count * 2);
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
  dr = object_nil;
  return ip_throw(ArgumentError, unbound_symbol);
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
  param_count = 0;
  if (!parse_required_params(params)) return FALSE;
  dr = gc_new_macro(cr, param_count, params, argv->cons.cdr);
  return TRUE;
}

DEFSP(f)
{
  object params;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_argv(BI_LIST, argv->cons.car, &params)) return FALSE;
  param_count = 0;
  if (!parse_required_params(params)) return FALSE;
  dr = gc_new_func(cr, param_count, params, argv->cons.cdr);
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
  while (e != object_nil) {
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
  o = object_nil;
  for (i = 0; i <= fp; i = next_fp(i)) {
    switch (fs_nth(i)) {
      case FUNC_FRAME:
        p = get_frame_var(i, 1);
        if (object_type(p) != CONS) o = gc_new_cons(p, o);
        else {
          f = p->cons.car;
          args = p->cons.cdr;
          named_proc = NULL;
          anonimous_proc = f;
          if (resolve_anonimous_proc()) f = named_proc;
          else f = anonimous_proc;
          o = gc_new_cons(gc_new_cons(f, args), o);
        }
        break;
      default:
        break;
    }
  }
  return o;
}

static object new_Error(enum error e, enum error_msg em)
{
  char *err, *msg;
  object o;
  o = gc_new_dict();
  err = error_name(e);
  map_put(o, object_class, gc_new_mem_from_cstr(SYMBOL, err));
  if ((msg = error_msg(em)) != NULL)
    map_put(o, object_message, gc_new_mem_from_cstr(STRING, msg));
  map_put(o, object_stack_trace, object_nil);
  return o;
}

static void trap(void)
{
  gen0(THROW_FRAME);
  switch (ip_trap_code) {
    case TRAP_ERROR:
      dr = new_Error(e, em);
      break;
    case TRAP_INTERRUPT:
      dr = new_Error(SystemExit, error_msg_nil);
      break;
    default:
      xassert(FALSE);
      break;
  }
  ip_trap_code = TRAP_NONE;
  e = error_nil;
  em = error_msg_nil;
}

// main

static void ip_main(object args)
{
  dr = object_nil;
  cr = object_toplevel;
  gen_eval_sequential_frame(args);
  while (fp != -1) {
    xassert(fp >= 0);
    if (ip_trap_code != TRAP_NONE) trap();
    if (cycle % IP_POLLING_INTERVAL == 0) gc_chance();
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
  gc_mark(dr);
  gc_mark(cr);
  gc_mark(object_toplevel);
  for (i = 0; i < sp; i++) gc_mark(fs[i]);
  gc_mark(object_toplevel);
  gc_mark(object_nil);
  gc_mark(object_true);
  gc_mark(object_key);
  gc_mark(object_opt);
  gc_mark(object_rest);
  gc_mark(object_quote);
  gc_mark(object_stack_trace);
  gc_mark(object_Class);
  gc_mark(object_Exception);
  gc_mark(object_class);
  gc_mark(object_symbol);
  gc_mark(object_super);
  gc_mark(object_features);
  gc_mark(object_fields);
  gc_mark(object_message);
}

int ip_start(object args)
{
  sp = 0;
  fp = -1;
  cycle = 0;
  xbarray_init(&bi_buf);
  ip_trap_code = TRAP_NONE;
  intr_init();
  ip_main(args);
  if (sint_p(dr)) return sint_val(dr);
  return 1;
}
