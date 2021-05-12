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
  if (sigaction(SIGINT, &sa, NULL) == -1) xerror("sigaction/failed.");
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
    xerror("SetConsoleCtrlHandler/failed.");
}

#endif

// error

int ip_trap_code;
static char *error_msg;

int ip_mark_error(char *msg)
{
  ip_trap_code = TRAP_ERROR;
  error_msg = msg;
  return FALSE;
}

static object new_Error(object cls, char *msg)
{
  object o;
  o = gc_new_dict();
  map_put(o, object_class, cls);
  if (msg != NULL) map_put(o, object_message, gc_new_mem_from(STRING, msg, strlen(msg)));
  return o;
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
#define   APPLY_FRAME            0x003
#define   APPLY_BUILTIN_FRAME    0x013
#define   ASSERT_FRAME           0x023
#define   BIND_HANDLER_FRAME     0x033
#define   BIND_FRAME             0x043
#define   BIND_PROPAGATION_FRAME 0x053
#define   EVAL_FRAME             0x062
#define   EVAL_ARGS_FRAME        0x074
#define   EVAL_SEQUENTIAL_FRAME  0x083
#define   GOTO_FRAME             0x092
#define   HANDLERS_FRAME         0x0a3
#define   IF_FRAME               0x0b3
#define   LABELS_FRAME           0x0c3
#define   FUNC_FRAME             0x0d4
#define   LET_FRAME              0x0e2
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

char *frame_name(int frame_type)
{
  switch (frame_type) {
    case APPLY_FRAME: return "APPLY_FRAME";
    case APPLY_BUILTIN_FRAME: return "APPLY_BUILTIN_FRAME";
    case ASSERT_FRAME: return "ASSERT_FRAME";
    case BIND_HANDLER_FRAME: return "BIND_HANDLER_FRAME";
    case BIND_FRAME: return "BIND_FRAME";
    case BIND_PROPAGATION_FRAME: return "BIND_PROPAGATION_FRAME";
    case EVAL_FRAME: return "EVAL_FRAME";
    case EVAL_ARGS_FRAME: return "EVAL_ARGS_FRAME";
    case EVAL_SEQUENTIAL_FRAME: return "EVAL_SEQUENTIAL_FRAME";
    case GOTO_FRAME: return "GOTO_FRAME";
    case HANDLERS_FRAME: return "HANDLERS_FRAME";
    case IF_FRAME: return "IF_FRAME";
    case LABELS_FRAME: return "LABELS_FRAME";
    case FUNC_FRAME: return "FUNC_FRAME";
    case LET_FRAME: return "LET_FRAME";
    case QUOTE_FRAME: return "QUOTE_FRAME";
    case RETURN_FRAME: return "RETURN_FRAME";
    case THROW_FRAME: return "THROW_FRAME";
    case UNWIND_PROTECT_FRAME: return "UNWIND_PROTECT_FRAME";
    default: xassert(FALSE); return NULL;
  }
}

static int fp;
static int sp;
static object fs[FRAME_STACK_SIZE];

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
}

static void exit1(void)
{
  char buf[MAX_STR_LEN];
  object o;
  fprintf(stderr, "%s", object_describe(map_get(reg[0], object_class), buf));
  if ((o = map_get(reg[0], object_message)) != NULL && o != object_nil)
    fprintf(stderr, " -- %s.", object_describe(o, buf));
  fprintf(stderr, "\n");
  o = map_get(reg[0], object_stack_trace);
  while (o != object_nil) {
    fprintf(stderr, "	at: %s\n", object_describe(o->cons.car, buf));
    o = o->cons.cdr;
  }
  exit(1);
}

static void gen(int frame_type)
{
  if (sp > FRAME_STACK_SIZE - STACK_GAP) ip_mark_error("stack over flow");
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
  else {
    gen1(EVAL_SEQUENTIAL_FRAME, args);
  }
}

static void gen_if_frame(object args)
{
  if (args == object_nil) return;
  if (args->cons.cdr != object_nil) gen1(IF_FRAME, args->cons.cdr);
  gen_eval_frame(args->cons.car);
}

static int same_symbol_keyword_p(object sym, object key)
{
  xassert(object_type_p(sym, SYMBOL) && object_type_p(key, KEYWORD));
  if (sym->mem.size != key->mem.size) return FALSE;
  return memcmp(sym->mem.elt, key->mem.elt, sym->mem.size) == 0;
}

static int valid_keyword_args(object params, object args)
{
  object p;
  while (args != object_nil) {
    if (!object_type_p(args->cons.car, KEYWORD))
      return ip_mark_error("expected keyword parameter");
    p = params;
    while (p != object_nil) {
      if (same_symbol_keyword_p(p->cons.car, args->cons.car)) break;
      p = p->cons.cdr;
    }
    if (p == object_nil)
      return ip_mark_error("undeclared keyword parameter");
    if ((args = args->cons.cdr) == object_nil)
      return ip_mark_error("expected keyword parameter value");
    args = args->cons.cdr;
  }
  return TRUE;
}

static int parse_args(void (*f)(object, object, object), object params
    , object args)
{
  object o, k, v;
  if (!list_p(params)) return ip_mark_error("illegal parameters");
  if (!list_p(args)) return ip_mark_error("illegal arguments");
  // parse required args
  while (params != object_nil) {
    if (object_type_p(params->cons.car, KEYWORD)) break;
    if (args == object_nil) {
      bi_argc_range(0, 1, 1);
      return FALSE;
    }
    if (object_type_p(params->cons.car, SYMBOL)) (*f)(reg[1], params->cons.car, args->cons.car);
    else if (!parse_args(f, params->cons.car, args->cons.car)) return FALSE;
    params = params->cons.cdr;
    args = args->cons.cdr;
  }
  // parse optional args
  if (params->cons.car == object_opt) {
    params = params->cons.cdr;
    while (params != object_nil) {
      if (object_type_p(params->cons.car, KEYWORD)) break;
      k = params->cons.car;
      if (args == object_nil) (*f)(reg[1], k, object_nil);
      else {
        (*f)(reg[1], k, args->cons.car);
        args = args->cons.cdr;
      }
      params = params->cons.cdr;
    }
  }
  // parse rest args
  if (params->cons.car == object_rest) {
    (*f)(reg[1], params->cons.cdr->cons.car, args);
    return TRUE;
  }
  // parse keyword args
  if (params->cons.car == object_key) {
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
  if (args != object_nil) {
    bi_argc_range(2, 1, 1);
    return FALSE;
  }
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

static void pop_apply_builtin_frame(void)
{
  object f, args;
  int (*function)(int, object, object *);
  f = get_frame_var(fp, 0);
  args = reg[0];
  function = f->builtin.u.function;
  pop_frame();
  if ((*function)(list_len(args), args, &(reg[0]))) return;
  gen2(FUNC_FRAME, reg[1], gc_new_cons(f->builtin.name, args));    // for stack trace
  if (error_msg == NULL) ip_mark_error("built-in function failed");
}

static void pop_assert_frame(void)
{
  if (reg[0] != object_nil) pop_frame();
  else ip_mark_error("assert failed");
}

static void pop_bind_frame(void)
{
  object o;
  o = get_frame_var(fp, 0);
  if (object_type_p(o, SYMBOL)) map_put(reg[1], o, reg[0]);
  else parse_args(&map_put, o, reg[0]);
  pop_frame();
}

static void pop_bind_handler_frame(void)
{
  int i;
  object cls_sym, handler, handlers;
  cls_sym = get_frame_var(fp, 0);
  pop_frame();
  if (!bi_arg_type(reg[0], FUNC, &handler)) return;
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
  if (object_type_p(o, SYMBOL)) map_put_propagation(reg[1], o, reg[0]);
  else parse_args(&map_put_propagation, o, reg[0]);
  pop_frame();
}

static int eval_symbol(object *result)
{
  object o;
  o = *result;
  if ((*result = map_get_propagation(reg[1], *result)) == NULL) {
    gen2(FUNC_FRAME, reg[1], o);    // for stack trace
    ip_mark_error("unbound symbol");
    return FALSE;
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
      if (object_type_p(operator, SYMBOL)) {
        if (!eval_symbol(&operator)) return;
      }
      switch (object_type(operator)) {
        case SPECIAL:
          special = operator->builtin.u.special;
          if ((*special)(list_len(args), args)) return;
          gen2(FUNC_FRAME, reg[1], gc_new_cons(operator->builtin.name, args));    // for stack trace
          return;
        case BUILTINFUNC:
          gen1(APPLY_BUILTIN_FRAME, operator);
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
          ip_mark_error("expected operator");
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
    case BUILTINFUNC:
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

static void pop_unwind_protect_frame(void)
{
  object body;
  body = get_frame_var(fp, 0);
  pop_frame();
  gen_eval_sequential_frame(body);
}

static void pop_goto_frame(void)
{
  int i;
  object o, label;
  i = fp;
  label = reg[0];
  while (fp > -1) {
    switch (fs_top()) {
      case LABELS_FRAME:
        o = get_frame_var(fp, 0);
        while (o != object_nil) {
          if (o->cons.car == label) {
            gen_eval_sequential_frame(o->cons.cdr);
            return;
          }
          o = o->cons.cdr;
        }
        ip_mark_error("undeclared label");
        set_fp(i);
        return;
      case UNWIND_PROTECT_FRAME:
        o = get_frame_var(fp, 0);
        pop_frame();
        gen0(GOTO_FRAME);
        gen1(QUOTE_FRAME, label);
        gen_eval_sequential_frame(o);
        return;
      default:
        pop_rewinding();
        break;
    }
  }
  ip_mark_error("expected labels context");
  set_fp(i);
  return;
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
  if (args == object_nil) {
    pop_frame();
    return;
  }
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
  object args;
  while (fp != -1) {
    switch (fs_top()) {
      case FUNC_FRAME:
        pop_func_frame();
        return;
      case UNWIND_PROTECT_FRAME: 
        args = get_frame_var(fp, 0);
        pop_frame();
        gen0(RETURN_FRAME);
        gen1(QUOTE_FRAME, reg[0]);
        gen_eval_sequential_frame(args);
        return;
      default:
        pop_rewinding();
        break;
    }
  }
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
        if (!object_type_p(p, CONS)) o = gc_new_cons(p, o);
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

static int pos_is_a_p(object o, object cls_sym);

static void pop_throw_frame(void)
{
  int i, j;
  object cls_sym, handler, handlers, body;
  i = fp;
  pop_frame();
  if (!pos_is_a_p(reg[0], object_Exception))
    reg[0] = new_Error(object_Error, "expected Exception object");
  if (map_get(reg[0], object_stack_trace) == NULL)
    map_put(reg[0], object_stack_trace, get_call_stack());
  while (fp > -1) {
    switch (fs_top()) {
      case UNWIND_PROTECT_FRAME:
        body = get_frame_var(fp, 0);
        pop_frame();
        gen0(THROW_FRAME);
        gen1(QUOTE_FRAME, reg[0]);
        gen_eval_sequential_frame(body);
        return;
      case HANDLERS_FRAME:
        handlers = get_frame_var(fp, 0);
        xassert(object_type_p(handlers, ARRAY));
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
  set_fp(i);
  exit1();
}

// fundamental built in functions

DEFUN(_3d_)
{
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  reg[0] = object_bool(object_eq_p(argv->cons.car, argv->cons.cdr->cons.car));
  return TRUE;
}

DEFUN(_3d__3d_)
{
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  reg[0] = object_bool(argv->cons.car == argv->cons.cdr->cons.car);
  return TRUE;
}

DEFUN(len)
{
  int len;
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if ((o = argv->cons.car) == object_nil) len = 0;
  else {
    switch (object_type(o)) {
      case CONS:
        len = list_len(o);
        break;
      case BYTES:
        len = o->mem.size;
        break;
      case STRING:
        if (!str_len(o, &len)) return FALSE;
        break;
      case ARRAY:
        len = o->array.size;
        break;
      case DICT:
        len = map_len(o);
      default:
        return bi_mark_type_error();
    }
  }
  reg[0] = gc_new_xint(len);
  return TRUE;
}

DEFUN(slice)
{
  int i, start, stop, len;
  object o;
  if (!bi_argc_range(argc, 1, 3)) return FALSE;
  reg[0] = object_nil;
  if ((o = argv->cons.car) == object_nil) return TRUE;
  if (argc < 2) start = 0;
  else if (!bi_spint((argv = argv->cons.cdr)->cons.car, &start)) return FALSE;
  if (argc < 3) stop = -1;
  else if (!bi_spint((argv = argv->cons.cdr)->cons.car, &stop)) return FALSE;
  else if (start > stop) return FALSE;
  switch (object_type(o)) {
    case CONS:
      for (i = 0; i < start; i++) {
        if ((o = o->cons.cdr) == object_nil) return TRUE;
      }
      if (o == object_nil || stop == -1) reg[0] = o;
      else {
        for (i = start; i < stop; i++) {
          reg[0] = gc_new_cons(o->cons.car, reg[0]);
          if ((o = o->cons.cdr) == object_nil) break;
        }
        reg[0] = list_reverse(reg[0]);
      }
      return TRUE;
    case BYTES:
      if (stop == -1) stop = o->mem.size;
      else if (stop > o->mem.size) return FALSE;
      reg[0] = gc_new_mem_from(BYTES, o->mem.elt + start, stop - start);
      return TRUE;
    case STRING:
      if (!str_slice(o, start, stop, &(reg[0]))) return FALSE;
      return TRUE;
    case ARRAY:
      if (stop == -1) stop = o->array.size;
      else if (stop > o->array.size) return FALSE;
      reg[0] = gc_new_array(len = stop - start);
      memcpy(reg[0]->array.elt, o->array.elt + start, sizeof(object) * len);
      return TRUE;
    default:
      return bi_mark_type_error();
  }
}

DEFUN(_5b__5d_)
{
  int i, byte;
  object o;
  if (!bi_argc_range(argc, 2, 3)) return FALSE;
  o = argv->cons.car;
  if (!bi_spint((argv = argv->cons.cdr)->cons.car, &i)) return FALSE;
  switch (object_type(o)) {
    case STRING:
      if (argc == 2) return str_slice(o, i, i + 1, result);
      return FALSE;
    case BYTES:
      if (i >= o->mem.size) return FALSE;
      if (argc == 2) *result = sint(LC(o->mem.elt + i));
      else {
        if (!bi_sint((*result = argv->cons.cdr->cons.car), &byte)) return FALSE;
        if (!byte_p(byte)) return FALSE;
        SC(o->mem.elt + i, byte);
      }
      return TRUE;
    case ARRAY:
      if (i >= o->array.size) return FALSE;
      if (argc == 2) *result = o->array.elt[i];
      else *result = o->array.elt[i] = argv->cons.cdr->cons.car;
      return TRUE;
    default:
      return FALSE;
  }
}

DEFUN(address)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = gc_new_xint((intptr_t)argv->cons.car);
  return TRUE;
}

DEFUN(_21_)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  *result = object_bool(argv->cons.car == object_nil);
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
  if (!bi_arg_list(argv->cons.cdr->cons.car, &args)) return FALSE;
  reg[0] = args;
  switch (object_type(argv->cons.car)) {
    case BUILTINFUNC:
      gen1(APPLY_BUILTIN_FRAME, argv->cons.car);
      return TRUE;
    case FUNC:
      gen1(APPLY_FRAME, argv->cons.car);
      return TRUE;
    default:
      return ip_mark_error("expected function");
  }
}

DEFUN(expand_2d_macro)
{
  object f, args;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  reg[0] = argv->cons.car;
  if (!object_type_p(reg[0], CONS)) return TRUE;
  f = reg[0]->cons.car;
  args = reg[0]->cons.cdr;
  if (object_type_p(f, SYMBOL)) {
    if ((f = map_get_propagation(reg[1], f)) == NULL) return TRUE;
  }
  if (!object_type_p(f, MACRO)) return TRUE;
  gen1(APPLY_FRAME, f);
  reg[0] = args;
  return TRUE;
}

DEFUN(bound_3f_)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &o)) return FALSE;
  reg[0] = object_bool(map_get_propagation(reg[1], o) != NULL);
  return TRUE;
}

static void trap(void)
{
  gen0(THROW_FRAME);
  switch (ip_trap_code) {
    case TRAP_ERROR:
      reg[0] = new_Error(object_Error, error_msg);
      break;
    case TRAP_INTERRUPT:
      reg[0] = new_Error(object_SystemExit, NULL);
      break;
    default:
      xassert(FALSE);
      break;
  }
  ip_trap_code = TRAP_NONE;
  error_msg = NULL;
}

// paren object system.

static int pos_object_p(object o)
{
  return object_type_p(o, DICT) && map_get(o, object_class) != NULL;
}

static int pos_class_p(object o)
{
  return object_type_p(o, DICT)
    && map_get(o, object_class) == object_Class
    && map_get(o, object_symbol) != NULL
    && map_get(o, object_super) != NULL
    && map_get(o, object_features) != NULL
    && map_get(o, object_fields) != NULL;
}

static int find_class(object cls_sym, object *result)
{
  if (!object_type_p(cls_sym, SYMBOL)) return FALSE;
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
  xassert(object_type_p(cls_sym, SYMBOL));
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
  xassert(object_type_p(cls_sym, SYMBOL));
  xassert(object_type_p(mtd_sym, SYMBOL));
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
  if (!pos_class_p(cls = argv->cons.cdr->cons.car)) {
    ip_mark_error("require Class instance");
    return FALSE;
  }
  reg[0] = object_bool(pos_is_a_p(o, map_get(cls, object_symbol)));
  return TRUE;
}

DEFUN(find_2d_class)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!object_type_p(argv->cons.car, SYMBOL)) return FALSE;
  if (!find_class(argv->cons.car, result)) *result = object_nil;
  return TRUE;
}

DEFUN(find_2d_method)
{
  object cls, cls_sym, mtd_sym, features;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &cls_sym)) return FALSE;
  if (!bi_arg_type(argv->cons.cdr->cons.car, SYMBOL, &mtd_sym)) return FALSE;
  while (TRUE) {
    // find class method
    if ((*result = find_class_method(cls_sym, mtd_sym)) != NULL) return TRUE;
    // find feature method
    if (!find_class(cls_sym, &cls)) return FALSE;
    features = map_get(cls, object_features);
    if (!list_p(features)) return FALSE;
    while (features != object_nil) {
      if ((*result = find_class_method(features->cons.car, mtd_sym)) != NULL) return TRUE;
      features = features->cons.cdr;
    }
    // super class
    if (!find_super_class(cls_sym, &cls)) return ip_mark_error("undeclared method");
    cls_sym = map_get(cls, object_symbol);
  }
}

DEFUN(cycle)
{
  reg[0] = gc_new_xint(cycle);
  return TRUE;
}

DEFUN(dump)
{
  dump_fs();
  return TRUE;
}

// special operator

static int param_count;

static int parse_rest_param(object params)
{
  switch (object_type(params->cons.car)) {
    case SYMBOL:
      param_count++;
      break;
    default:
      return FALSE;
  }
  return params->cons.cdr == object_nil;
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
        return FALSE;
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
        return FALSE;
      case SYMBOL:
        params = params->cons.cdr;
        param_count++;
        break;
      default:
        return FALSE;
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
        return FALSE;
      case CONS:
        if (!parse_required_params(params->cons.car)) return FALSE;
        params = params->cons.cdr;
        break;
      case SYMBOL:
        params = params->cons.cdr;
        param_count++;
        break;
      default:
        return FALSE;
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
      break;
    case CONS:
      if (!parse_required_params(o->cons.car)) return FALSE;
      break;
    default:
      return ip_mark_error("unexpected binding expression");
  }
  if ((args = args->cons.cdr) == object_nil)
    return ip_mark_error("missing binding value");
  if (!gen_bind_frames(frame_type, args->cons.cdr)) return FALSE;
  gen1(frame_type, o->cons.car);
  gen0(EVAL_FRAME);
  gen1(QUOTE_FRAME, o->cons.cdr->cons.car);
  return TRUE;
}

static int count_let_sym(object params)
{
  while (params != object_nil) {
    switch (object_type(params->cons.car)) {
      case CONS: parse_required_params(params->cons.car); break;
      case SYMBOL: param_count++; break;
      case KEYWORD: break;
      default: xassert(FALSE);
    }
    params = params->cons.cdr->cons.cdr;
  }
  return param_count;
}

DEFSP(let)
{
  object args;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &args)) return FALSE;
  if (args == object_nil) gen_eval_sequential_frame(argv->cons.cdr);
  else {
    gen0(LET_FRAME);
    gen_eval_sequential_frame(argv->cons.cdr);
    if (!gen_bind_frames(BIND_FRAME, args)) return FALSE;
    param_count = 0;
    reg[1] = gc_new_env(reg[1], count_let_sym(args) * 2);
  }
  return TRUE;
}

DEFSP(dynamic)
{
  int i;
  object e, s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &s)) return FALSE;
  i = fp;
  e = reg[1];
  if ((reg[0] = map_get(e, s)) != NULL) return TRUE;
  while ((i = prev_fp(i)) != -1) {
    switch (fs_nth(i)) {
      case LET_FRAME:
        e = e->map.top;
        break;
      case FUNC_FRAME:
        e = get_frame_var(i, 0);
        break;
      default:
        continue;
    }
    if ((reg[0] = map_get(e, s)) != NULL) return TRUE;
  }
  return FALSE;
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
  if (!bi_arg_type(argv->cons.car, SYMBOL, &o)) return FALSE;
  gen1(BIND_PROPAGATION_FRAME, o);
  argv = argv->cons.cdr;
  if (!bi_arg_list(argv->cons.car, &params)) return FALSE;
  param_count = 0;
  if (!parse_required_params(params))
    return ip_mark_error("illegal macro parameter list");
  reg[0] = gc_new_macro(reg[1], param_count, params, argv->cons.cdr);
  return TRUE;
}

DEFSP(f)
{
  object params;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &params)) return FALSE;
  param_count = 0;
  if (!parse_required_params(params))
    return ip_mark_error("illegal function parameter list");
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

DEFSP(labels)
{
  gen1(LABELS_FRAME, argv);
  gen_eval_sequential_frame(argv);
  return TRUE;
}

DEFSP(goto)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, KEYWORD, &o)) return FALSE;
  gen0(GOTO_FRAME);
  reg[0] = o;
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
  if (!bi_arg_list(argv->cons.car, &params)) return FALSE;
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
      case APPLY_FRAME: pop_apply_frame(); break;
      case APPLY_BUILTIN_FRAME: pop_apply_builtin_frame(); break;
      case ASSERT_FRAME: pop_assert_frame(); break;
      case BIND_FRAME: pop_bind_frame(); break;
      case BIND_HANDLER_FRAME: pop_bind_handler_frame(); break;
      case BIND_PROPAGATION_FRAME: pop_bind_propagation_frame(); break;
      case EVAL_FRAME: pop_eval_frame(); break;
      case EVAL_ARGS_FRAME: pop_eval_args_frame(); break;
      case EVAL_SEQUENTIAL_FRAME: pop_eval_sequential_frame(); break;
      case GOTO_FRAME: pop_goto_frame(); break;
      case HANDLERS_FRAME: pop_frame(); break;
      case IF_FRAME: pop_if_frame(); break;
      case LABELS_FRAME: pop_frame(); break;
      case FUNC_FRAME: pop_func_frame(); break;
      case LET_FRAME: pop_let_frame(); break;
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
  gc_mark(object_Error);
  gc_mark(object_SystemExit);
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
