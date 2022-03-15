// interpreter.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

/*
 * virtual machine registers
 *   0 -- instruction argument.
 *   1 -- current environment.
 */
#define REG_SIZE 2
static object reg[REG_SIZE];

static long cycle;

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
    case IndexError: return "IndexError";
    case ArithmeticError: return "ArithmeticError";
    case StateError: return "StateError";
    case UnicodeError: return "UnicodeError";
    case OSError: return "OSError";
    case SyntaxError: return "SyntaxError";
    default: xassert(FALSE); return NULL;
  }
};

static char *error_msg(enum error_msg em) {
  switch (em) {
    case assert_failed: return "assert failed";
    case built_in_failed: return "built-in function failed";
    case clip_failed: return "clip failed";
    case connection_failed: return "connection failed";
    case division_by_zero: return "division by zero";
    case expected_array: return "expected array";
    case expected_binding_value: return "expected binding value";
    case expected_built_in_operator: return "expected built-in operator";
    case expected_byte: return "expected byte";
    case expected_bytes: return "expected bytes";
    case expected_bytes_like: return "expected bytes like object";
    case expected_collection: return "expected collection";
    case expected_comparable: return "expected comparable";
    case expected_cons: return "expected cons";
    case expected_dict: return "expected dictionary";
    case expected_function: return "expected function";
    case expected_function_macro: return "expected function or macro";
    case expected_instance_of_Class_class: return "expected instance of Class class";
    case expected_instance_of_Exception_class: return "expected instance of Exception class";
    case expected_integer: return "expected integer";
    case expected_keyword: return "expected keyword";
    case expected_keyword_parameter_value: return "expected keyword parameter value";
    case expected_list: return "expected list";
    case expected_loop_context: return "expected loop context";
    case expected_mutable_sequence: return "expected mutable sequence";
    case expected_number: return "expected number";
    case expected_operator: return "expected operator";
    case expected_positive_integer: return "expected positive integer";
    case expected_positive_integer_or_sequence: return "expected positive integer or sequence";
    case expected_sequence: return "expected sequence";
    case expected_string: return "expected string";
    case expected_symbol: return "expected symbol";
    case expected_symbol_keyword: return "expected symbol or keyword";
    case fgetc_failed: return "fgetc failed";
    case fopen_failed: return "fopen failed";
    case fp_failed: return "fp failed";
    case fputc_failed: return "fputc failed";
    case fread_failed: return "fread failed";
    case fseek_failed: return "fseek failed";
    case ftell_failed: return "ftell failed";
    case fwrite_failed: return "fwrite failed";
    case gethostname_failed: return "gethostname failed";
    case index_out_of_range: return "index out of range";
    case invalid_args: return "invalid arguments";
    case invalid_binding_expr: return "invalid binding expression";
    case invalid_utf8_byte_sequence: return "invalid utf8 byte sequence";
    case numeric_overflow: return "numeric overflow";
    case readdir_failed: return "readdir failed";
    case recv_failed: return "recv failed";
    case send_failed: return "send failed";
    case stack_over_flow: return "stack over flow";
    case stat_failed: return "stat failed";
    case too_few_arguments: return "too few arguments";
    case too_many_arguments: return "too many arguments";
    case unbound_symbol: return "unbound symbol";
    case undeclared_class: return "undeclared class";
    case undeclared_keyword_param: return "undeclared keyword parameter";
    case unexpected_keyword_parameter: return "unexpected keyword parameter";
    case error_msg_nil: return NULL;
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
#define   ASSERT_FRAME           0x023
#define   BIND_FRAME             0x033
#define   BIND_HANDLER_FRAME     0x043
#define   BIND_PROPAGATION_FRAME 0x053
#define   BREAK_FRAME            0x062
#define   CONTINUE_FRAME         0x072
#define   EVAL_ARGS_FRAME        0x084
#define   EVAL_FRAME             0x092
#define   EVAL_SEQUENTIAL_FRAME  0x0a3
#define   FUNC_FRAME             0x0b4
#define   HANDLERS_FRAME         0x0c3
#define   IF_FRAME               0x0d3
#define   LET_FRAME              0x0e2
#define   LOOP_FRAME             0x0f3
#define   QUOTE_FRAME            0x103
#define   RETURN_FRAME           0x112
#define   THROW_FRAME            0x122
#define   UNWIND_PROTECT_FRAME   0x133

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
    case ASSERT_FRAME: return "ASSERT_FRAME";
    case BIND_FRAME: return "BIND_FRAME";
    case BIND_HANDLER_FRAME: return "BIND_HANDLER_FRAME";
    case BIND_PROPAGATION_FRAME: return "BIND_PROPAGATION_FRAME";
    case BREAK_FRAME: return "BREAK_FRAME";
    case CONTINUE_FRAME: return "CONTINUE_FRAME";
    case EVAL_ARGS_FRAME: return "EVAL_ARGS_FRAME";
    case EVAL_FRAME: return "EVAL_FRAME";
    case EVAL_SEQUENTIAL_FRAME: return "EVAL_SEQUENTIAL_FRAME";
    case FUNC_FRAME: return "FUNC_FRAME";
    case HANDLERS_FRAME: return "HANDLERS_FRAME";
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
  reg[0] = o;
  switch (object_type(reg[0])) {
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
  if (args == object_nil) reg[0] = object_nil;
  else {
    gen2(EVAL_ARGS_FRAME, args->cons.cdr, object_nil);
    gen_eval_frame(args->cons.car);
  }
}

static void gen_eval_sequential_frame(object args)
{
  if (args == object_nil) reg[0] = object_nil;
  else gen1(EVAL_SEQUENTIAL_FRAME, args);
}

static void gen_if_frame(object args)
{
  if (args == object_nil) return;
  if (args->cons.cdr != object_nil) gen1(IF_FRAME, args->cons.cdr);
  gen_eval_frame(args->cons.car);
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
      (*f)(reg[1], params->cons.car, args->cons.car);
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
      if (args == object_nil) (*f)(reg[1], k, object_nil);
      else {
        (*f)(reg[1], k, args->cons.car);
        args = args->cons.cdr;
      }
      params = params->cons.cdr;
    }
  }
  // rest args
  if (params->cons.car == object_rest) {
    (*f)(reg[1], params->cons.cdr->cons.car, args);
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
      (*f)(reg[1], k, v);
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
  reg[1] = get_frame_var(fp, 0);
  pop_frame();
}

static void pop_let_frame(void)
{
  reg[1] = reg[1]->map.top;
  pop_frame();
}

static void pop_apply_frame(void)
{
  object func, trace;
  func = get_frame_var(fp, 0);
  pop_frame();
  trace = gc_new_cons(func, reg[0]);
  // optimize tail recursion
  if (fs_top() == FUNC_FRAME) set_frame_var(fp, 1, trace);
  else gen2(FUNC_FRAME, reg[1], trace);
  if (func->proc.param_count == 0) reg[1] = func->proc.env;
  else reg[1] = gc_new_env(func->proc.env, func->proc.param_count * 2);
  gen_eval_sequential_frame(func->proc.body);
  parse_args(&map_put, func->proc.params, reg[0]);
}

static void pop_apply_built_in_frame(void)
{
  object f, args;
  int (*function)(int, object, object *);
  f = get_frame_var(fp, 0);
  args = reg[0];
  function = f->native.u.function;
  pop_frame();
  if ((*function)(list_len(args), args, &(reg[0]))) return;
  if (ip_trap_code == TRAP_NONE) ip_throw(Error, built_in_failed);
  // trace
  gen2(FUNC_FRAME, reg[1], gc_new_cons(f->native.name, args));
}

static void pop_assert_frame(void)
{
  if (reg[0] != object_nil) pop_frame();
  else ip_throw(StateError, assert_failed);
}

static void pop_bind_frame(void)
{
  object o;
  o = get_frame_var(fp, 0);
  if (object_type(o) == SYMBOL) map_put(reg[1], o, reg[0]);
  else parse_args(&map_put, o, reg[0]);
  pop_frame();
}

static void pop_bind_handler_frame(void)
{
  int i;
  object cls_sym, handler, handlers;
  cls_sym = get_frame_var(fp, 0);
  pop_frame();
  if (!bi_func(reg[0], &handler)) return;
  for (i = fp; i > -1; i = prev_fp(i)) {
    if (sint_val(fs[i]) != HANDLERS_FRAME) continue;
    handlers = get_frame_var(i, 0);
    for (i = 0; i < handlers->array.size; i += 2) {
      if (handlers->array.elt[i] != object_nil) continue;
      handlers->array.elt[i] = cls_sym;
      handlers->array.elt[i + 1] = handler;
      return;
    }
    xassert(FALSE);
  }
  xassert(FALSE);
}

static void pop_bind_propagation_frame(void)
{
  object o;
  o = get_frame_var(fp, 0);
  if (object_type(o) == SYMBOL) map_put_propagation(reg[1], o, reg[0]);
  else parse_args(&map_put_propagation, o, reg[0]);
  pop_frame();
}

static int eval_symbol(object *result)
{
  object o;
  o = *result;
  if ((*result = map_get_propagation(reg[1], *result)) == NULL) {
    // trace
    gen2(FUNC_FRAME, reg[1], o);
    return ip_throw(StateError, unbound_symbol);
  }
  return TRUE;
}

static void pop_eval_frame(void)
{
  object args, operator;
  int (*special)(int, object);
  pop_frame();
  switch (object_type(reg[0])) {
    case SYMBOL:
      eval_symbol(&(reg[0]));
      return;
    case CONS:
      operator = reg[0]->cons.car;
      args = reg[0]->cons.cdr;
      if (object_type(operator) == SYMBOL) {
        if (!eval_symbol(&operator)) return;
      }
      switch (object_type(operator)) {
        case SPECIAL:
          special = operator->native.u.special;
          if ((*special)(list_len(args), args)) return;
          // tarace
          gen2(FUNC_FRAME, reg[1], gc_new_cons(operator->native.name, args));
          return;
        case BFUNC:
          gen1(APPLY_BUILT_IN_FRAME, operator);
          gen_eval_args_frame(args);
          return;
        case MACRO:
          gen0(EVAL_FRAME);
          gen1(APPLY_FRAME, operator);
          reg[0] = args;
          return;
        case FUNC:
          gen1(APPLY_FRAME, operator);
          gen_eval_args_frame(args);
          return;
        default:
          ip_throw(StateError, expected_operator);
          // trace
          gen2(FUNC_FRAME, reg[1], reg[0]);
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
        reg[0] = object_nil;
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
  gen1(QUOTE_FRAME, reg[0]);
  gen_eval_sequential_frame(body);
}

static void pop_eval_args_frame(void)
{
  object rest, acc;
  rest = get_frame_var(fp, 0);
  acc = gc_new_cons(reg[0], get_frame_var(fp, 1));
  set_frame_var(fp, 1, acc);
  if (rest == object_nil) {
    pop_frame();
    reg[0] = list_reverse(acc);
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
  if (reg[0] != object_nil) gen_eval_frame(args->cons.car);
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
        gen1(QUOTE_FRAME, reg[0]);
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
  int i, j;
  object o, cls_sym, handler, handlers;
  i = fp;
  pop_frame();
  if (!pos_is_a_p(reg[0], object_Exception)) {
    ip_throw(ArgumentError, expected_instance_of_Exception_class);
#ifndef NDEBUG
    if (map_get(object_toplevel, gc_new_mem_from(SYMBOL, "boot", 4)) == NULL) dump_fs();
#endif
    return;
  }
  if (map_get(reg[0], object_stack_trace) == object_nil)
    map_put(reg[0], object_stack_trace, get_call_stack());
  while (fp != -1) {
    switch (fs_top()) {
      case UNWIND_PROTECT_FRAME:
        o = get_frame_var(fp, 0);
        pop_frame();
        gen0(THROW_FRAME);
        gen1(QUOTE_FRAME, reg[0]);
        gen_eval_sequential_frame(o);
        return;
      case HANDLERS_FRAME:
        handlers = get_frame_var(fp, 0);
        xassert(object_type(handlers) == ARRAY);
        pop_frame();
        for (j = 0; j < handlers->array.size; j += 2) {
          cls_sym = handlers->array.elt[j];
          handler = handlers->array.elt[j + 1];
          if (!pos_is_a_p(reg[0], cls_sym)) continue;
          gen1(APPLY_FRAME, handler);
          reg[0] = gc_new_cons(reg[0], object_nil);
          return;
        }
        break;
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
  if (!bi_list(argv->cons.cdr->cons.car, &args)) return FALSE;
  reg[0] = args;
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
  reg[0] = argv->cons.car;
  if (object_type(reg[0]) != CONS) return TRUE;
  f = reg[0]->cons.car;
  args = reg[0]->cons.cdr;
  if (object_type(f) == SYMBOL) {
    if ((f = map_get_propagation(reg[1], f)) == NULL) return TRUE;
  }
  if (object_type(f) != MACRO) return TRUE;
  gen1(APPLY_FRAME, f);
  reg[0] = args;
  return TRUE;
}

DEFUN(bound_3f_)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_symbol(argv->cons.car, &o)) return FALSE;
  reg[0] = object_bool(map_get_propagation(reg[1], o) != NULL);
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
  if ((*result = map_get_propagation(reg[1], cls_sym)) == NULL) return FALSE;
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
  return map_get_propagation(reg[1], gc_new_mem_from(SYMBOL, bi_buf.elt, bi_buf.size));
}

DEFUN(is_2d_a_3f_)
{
  object o, cls;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  o = argv->cons.car;
  if (!pos_class_p(cls = argv->cons.cdr->cons.car))
    return ip_throw(ArgumentError, expected_instance_of_Class_class);
  reg[0] = object_bool(pos_is_a_p(o, map_get(cls, object_symbol)));
  return TRUE;
}

DEFUN(find_2d_class)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_symbol(argv->cons.car, &o)) return FALSE;
  if (!find_class(o, result)) *result = object_nil;
  return TRUE;
}

DEFUN(find_2d_method)
{
  object cls, cls_sym, mtd_sym, features;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_symbol(argv->cons.car, &cls_sym)) return FALSE;
  if (!bi_symbol(argv->cons.cdr->cons.car, &mtd_sym)) return FALSE;
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

DEFUN(cycle)
{
  reg[0] = gc_new_xint(cycle);
  return TRUE;
}

DEFUN(exit)
{
  int sc;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cint(argv->cons.car, &sc)) return FALSE;
  exit(sc);
  return TRUE;
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
  object args;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  if (!bi_list(argv->cons.car, &args)) return FALSE;
  if (args == object_nil) gen_eval_sequential_frame(argv->cons.cdr);
  else {
    gen0(LET_FRAME);
    gen_eval_sequential_frame(argv->cons.cdr);
    param_count = 0;
    if (!gen_bind_frames(BIND_FRAME, args)) return FALSE;
    reg[1] = gc_new_env(reg[1], param_count * 2);
  }
  return TRUE;
}

DEFSP(dynamic)
{
  int i;
  object e, s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_symbol(argv->cons.car, &s)) return FALSE;
  i = fp;
  e = reg[1];
  if ((reg[0] = map_get(e, s)) != NULL) return TRUE;
  while ((i = prev_fp(i)) != -1) {
    switch (fs_nth(i)) {
      case LET_FRAME: e = e->map.top; break;
      case FUNC_FRAME: e = get_frame_var(i, 0); break;
      default: continue;
    }
    if ((reg[0] = map_get(e, s)) != NULL) return TRUE;
  }
  reg[0] = object_nil;
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
  if (!bi_symbol(argv->cons.car, &o)) return FALSE;
  gen1(BIND_PROPAGATION_FRAME, o);
  argv = argv->cons.cdr;
  if (!bi_list(argv->cons.car, &params)) return FALSE;
  param_count = 0;
  if (!parse_required_params(params)) return FALSE;
  reg[0] = gc_new_macro(reg[1], param_count, params, argv->cons.cdr);
  return TRUE;
}

DEFSP(f)
{
  object params;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_list(argv->cons.car, &params)) return FALSE;
  param_count = 0;
  if (!parse_required_params(params)) return FALSE;
  reg[0] = gc_new_func(reg[1], param_count, params, argv->cons.cdr);
  return TRUE;
}

DEFSP(quote)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  reg[0] = argv->cons.car;
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
  object params;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_list(argv->cons.car, &params)) return FALSE;
  gen1(HANDLERS_FRAME, gc_new_array(list_len(params)));
  gen_eval_sequential_frame(argv->cons.cdr);
  return gen_bind_frames(BIND_HANDLER_FRAME, params);
}

DEFSP(return)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  gen0(RETURN_FRAME);
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

DEFSP(assert)
{
#ifndef NDEBUG
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  gen1(ASSERT_FRAME, argv->cons.car);
  gen_eval_frame(argv->cons.car);
#endif
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
  e = reg[1];
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
      case ASSERT_FRAME:
        p = get_frame_var(i, 0);
        o = gc_new_cons(p, o);
        break;
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
  map_put(o, object_class, gc_new_mem_from(SYMBOL, err, strlen(err)));
  if ((msg = error_msg(em)) != NULL)
    map_put(o, object_message, gc_new_mem_from(STRING, msg, strlen(msg)));
  map_put(o, object_stack_trace, object_nil);
  map_put(o, object_status_cd, gc_new_xint(1));
  return o;
}

static void trap(void)
{
  gen0(THROW_FRAME);
  switch (ip_trap_code) {
    case TRAP_ERROR:
      reg[0] = new_Error(e, em);
      break;
    case TRAP_INTERRUPT:
      reg[0] = new_Error(SystemExit, error_msg_nil);
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
  reg[0] = object_nil;
  reg[1] = object_toplevel;
  gen_eval_sequential_frame(args);
  while (fp != -1) {
    xassert(fp >= 0);
    if (ip_trap_code != TRAP_NONE) trap();
    if (cycle % IP_POLLING_INTERVAL == 0) gc_chance();
    switch (fs_top()) {
      case APPLY_BUILT_IN_FRAME: pop_apply_built_in_frame(); break;
      case APPLY_FRAME: pop_apply_frame(); break;
      case ASSERT_FRAME: pop_assert_frame(); break;
      case BIND_FRAME: pop_bind_frame(); break;
      case BIND_HANDLER_FRAME: pop_bind_handler_frame(); break;
      case BIND_PROPAGATION_FRAME: pop_bind_propagation_frame(); break;
      case BREAK_FRAME: pop_break_continue_frame(TRUE); break;
      case CONTINUE_FRAME: pop_break_continue_frame(FALSE); break;
      case EVAL_ARGS_FRAME: pop_eval_args_frame(); break;
      case EVAL_FRAME: pop_eval_frame(); break;
      case EVAL_SEQUENTIAL_FRAME: pop_eval_sequential_frame(); break;
      case FUNC_FRAME: pop_func_frame(); break;
      case HANDLERS_FRAME: pop_frame(); break;
      case IF_FRAME: pop_if_frame(); break;
      case LET_FRAME: pop_let_frame(); break;
      case LOOP_FRAME: gen_eval_sequential_frame(get_frame_var(fp, 0)); break;
      case QUOTE_FRAME: reg[0] = get_frame_var(fp, 0); pop_frame(); break;
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
  for (i = 0; i < REG_SIZE; i++) gc_mark(reg[i]);
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

void ip_start(object args)
{
  sp = 0;
  fp = -1;
  cycle = 0;
  xbarray_init(&bi_buf);
  ip_trap_code = TRAP_NONE;
  intr_init();
  ip_main(args);
}
