// interpreter

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "splay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

/*
 * registers:
 *   0 -- instruction argument.
 *   1 -- current environment.
 */
#define REG_SIZE 2
static object reg[REG_SIZE];

static long cycle;

// error

int ip_trap_code;
static char *error_msg;

void ip_mark_error(char *msg)
{
  ip_trap_code = TRAP_ERROR;
  error_msg = msg;
}

static void mark_illegal_args()
{
  ip_mark_error("illegal parameter list");
}

// symbol

static object symbol_find(object e, object s)
{
  xassert(object_type_p(e, ENV));
  return splay_find(&e->env.binding, s);
}

static object symbol_find_propagation(object e, object s)
{
  object v;
  while (e != object_nil) {
    if ((v = symbol_find(e, s)) != NULL) return v;
    e = e->env.top;
  }
  return NULL;
}

static void symbol_bind(object e, object s, object v)
{
  object o;
  xassert(object_type_p(e, ENV) && object_type_p(s, SYMBOL));
  if ((o = splay_find(&e->env.binding, s)) != NULL) {
    if (object_type_p(o, SPECIAL)) {
      ip_mark_error("special operator could not bind");
      return;
    }
  }
  splay_replace(&e->env.binding, s, v);
}

static void symbol_bind_propagation(object e, object s, object v)
{
  while (e != object_toplevel) {
    if (symbol_find(e, s) != NULL) {
      symbol_bind(e, s, v);
      return;
    }
    e = e->env.top;
  }
  symbol_bind(object_toplevel, s, v);
}

// stack frame

#define STACK_GAP 10
#define FRAME_STACK_SIZE 10000

/*
 * frame structure.
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

static int fp;
static int sp;

static object fs[FRAME_STACK_SIZE];

// frame_type
#define FRAME_SIZE_MASK          0x0000000f
#define   APPLY_FRAME            0x00000003
#define   APPLY_BUILTIN_FRAME    0x00000013
#define   ASSERT_FRAME           0x00000022
#define   BIND_HANDLER_FRAME     0x00000033
#define   BIND_FRAME             0x00000043
#define   BIND_PROPAGATION_FRAME 0x00000053
#define   EVAL_FRAME             0x00000062
#define   EVAL_ARGS_FRAME        0x00000074
#define   EVAL_SEQUENTIAL_FRAME  0x00000083
#define   FETCH_OPERATOR_FRAME   0x00000093
#define   GOTO_FRAME             0x00000102
#define   HANDLERS_FRAME         0x00000113
#define   IF_FRAME               0x00000123
#define   LABELS_FRAME           0x00000133
#define   LAMBDA_FRAME           0x00000144
#define   LET_FRAME              0x00000153
#define   QUOTE_FRAME            0x00000163
#define   RETURN_FRAME           0x00000172
#define   THROW_FRAME            0x00000182
#define   UNWIND_PROTECT_FRAME   0x00000193

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
    case FETCH_OPERATOR_FRAME: return "FETCH_OPERATOR_FRAME";
    case GOTO_FRAME: return "GOTO_FRAME";
    case HANDLERS_FRAME: return "HANDLERS_FRAME";
    case IF_FRAME: return "IF_FRAME";
    case LABELS_FRAME: return "LABELS_FRAME";
    case LAMBDA_FRAME: return "LAMBDA_FRAME";
    case LET_FRAME: return "LET_FRAME";
    case QUOTE_FRAME: return "QUOTE_FRAME";
    case RETURN_FRAME: return "RETURN_FRAME";
    case THROW_FRAME: return "THROW_FRAME";
    case UNWIND_PROTECT_FRAME: return "UNWIND_PROTECT_FRAME";
    default: xassert(FALSE); return NULL;
  }
}

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

static void dump_fs(void)
{
  int i, j, frame_type;
  char buf[MAX_STR_LEN];
  for (i = 0; i <= fp; i = next_fp(i)) {
    frame_type = sint_val(fs[i]);
    printf("+-----------------------------\n");
    printf("|%d: %s\n", i, frame_name(frame_type));
    printf("|%d: %d\n", i + 1, sint_val(fs[i + 1]));
    for (j = 0; j < frame_size(frame_type) - 2; j++) {
      printf("|%d: %s\n", i + j + 2, object_describe(get_frame_var(i, j), buf));
    }
  }
}

static void exit1(void)
{
  char buf[MAX_STR_LEN];
  object o;
  o = reg[0]->cons.cdr;
  printf("%s", object_describe(o->cons.car, buf));
  o = o->cons.cdr->cons.cdr;
  if (o->cons.car != object_nil)
    printf(" -- %s.", object_describe(o->cons.car, buf));
  printf("\n");
  while (o->cons.car != object_stack_trace) o = o->cons.cdr;
  o = o->cons.cdr->cons.car;
  while (o != object_nil) {
    printf("	at: %s\n", object_describe(o->cons.car, buf));
    o = o->cons.cdr;
  }
  dump_fs();
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

static void gen_if_frame(object args)
{
  if (args == object_nil) return;
  if (args->cons.cdr != object_nil) gen1(IF_FRAME, args->cons.cdr);
  gen_eval_frame(args->cons.car);
}

static int same_symbol_keyword_p(object sym, object key)
{
  xassert(object_type_p(sym, SYMBOL) && object_type_p(key, KEYWORD));
  if (sym->bytes.size != key->bytes.size) return FALSE;
  return memcmp(sym->bytes.elt, key->bytes.elt, sym->bytes.size) == 0;
}

static int valid_keyword_args(object params, object args)
{
  object p;
  while (args != object_nil) {
    if (!object_type_p(args->cons.car, KEYWORD)) {
      ip_mark_error("expected keyword parameter");
      return FALSE;
    }
    p = params;
    while (p != object_nil) {
      if (same_symbol_keyword_p(p->cons.car, args->cons.car)) break;
      p = p->cons.cdr;
    }
    if (p == object_nil) {
      ip_mark_error("undeclared keyword parameter");
      return FALSE;
    }
    if ((args = args->cons.cdr) == object_nil) {
      ip_mark_error("expected keyword parameter value");
      return FALSE;
    }
    args = args->cons.cdr;
  }
  return TRUE;
}

static int parse_args(object params, object args)
{
  object o, k, v;
  // parse required args
  while (params != object_nil) {
    if (object_type_p(params->cons.car, KEYWORD)) break;
    if (args == object_nil) {
      bi_argc_range(0, 1, 1);
      return FALSE;
    }
    if (object_type_p(params->cons.car, SYMBOL))
      symbol_bind(reg[1], params->cons.car, args->cons.car);
    else {
      if (!object_list_p(args->cons.car)) return FALSE;
      if (!parse_args(params->cons.car, args->cons.car)) return FALSE;
    }
    params = params->cons.cdr;
    args = args->cons.cdr;
  }
  // parse optional args
  if (params->cons.car == object_opt) {
    params = params->cons.cdr;
    while (params != object_nil) {
      if (object_type_p(params->cons.car, KEYWORD)) break;
      k = params->cons.car;
      if (args == object_nil) symbol_bind(reg[1], k, object_nil);
      else {
        symbol_bind(reg[1], k, args->cons.car);
        args = args->cons.cdr;
      }
      params = params->cons.cdr;
    }
  }
  // parse rest args
  if (params->cons.car == object_rest) {
    symbol_bind(reg[1], params->cons.cdr->cons.car, args);
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
      symbol_bind(reg[1], k, v);
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

static void pop_lambda_frame(void)
{
  reg[1] = get_frame_var(fp, 0);
  pop_frame();
}

static void pop_let_frame(void)
{
  reg[1] = reg[1]->env.top;
  pop_frame();
}

static void pop_apply_frame(void)
{
  object operator;
  operator = get_frame_var(fp, 0);
  pop_frame();
  // optimize tail recursion
  if (fs_top() == LAMBDA_FRAME) set_frame_var(fp, 1, operator);
  else gen2(LAMBDA_FRAME, reg[1], operator);
  reg[1] = gc_new_env(operator->lambda.env);
  gen1(EVAL_SEQUENTIAL_FRAME, operator->lambda.body);
  parse_args(operator->lambda.params, reg[0]);
}

static void pop_apply_builtin_frame(void)
{
  object args;
  int (*function)(int, object, object *);
  args = reg[0];
  function = get_frame_var(fp, 0)->builtin.u.function;
  pop_frame();
  if ((*function)(object_list_len(args), args, &(reg[0]))) return;
  if (error_msg == NULL) ip_mark_error("built-in function failed");
}

static void pop_assert_frame(void)
{
  pop_frame();
  if (reg[0] == object_nil) ip_mark_error("assert failed");
}

static void pop_bind_frame(void)
{
  symbol_bind(reg[1], get_frame_var(fp, 0), reg[0]);
  pop_frame();
}

static void pop_bind_handler_frame(void)
{
  int i;
  object cls_sym, handler, handlers;
  cls_sym = get_frame_var(fp, 0);
  pop_frame();
  if (!bi_arg_type(reg[0], LAMBDA, &handler)) return;
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
  symbol_bind_propagation(reg[1], get_frame_var(fp, 0), reg[0]);
  pop_frame();
}

static void pop_eval_frame(void)
{
  object s;
  pop_frame();
  switch (object_type(reg[0])) {
    case SYMBOL:
      if ((s = symbol_find_propagation(reg[1], reg[0])) == NULL) {
        gen2(LAMBDA_FRAME, reg[1], reg[0]);    // for stack trace
        ip_mark_error("unbind symbol");
        return;
      }
      reg[0] = s;
      return;
    case CONS:
      gen1(FETCH_OPERATOR_FRAME, reg[0]->cons.cdr);
      gen_eval_frame(reg[0]->cons.car);
      return;
    case MACRO:
    case LAMBDA:
    case FUNCITON:
    case SPECIAL:
    case SINT:
    case XINT:
    case XFLOAT:
    case KEYWORD:
    case STRING:
    case BYTES:
    case ARRAY:
      return;
    default:
      xassert(FALSE);
      return;
  }
}

static void pop_rewinding(void)
{
  switch (fs_top()) {
    case LAMBDA_FRAME: pop_lambda_frame(); break;
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
  gen1(EVAL_SEQUENTIAL_FRAME, body);
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
            gen1(EVAL_SEQUENTIAL_FRAME, o->cons.cdr);
            return;
          }
          o = o->cons.cdr;
        }
        ip_mark_error("label not found");
        set_fp(i);
        return;
      case UNWIND_PROTECT_FRAME:
        o = get_frame_var(fp, 0);
        pop_frame();
        gen0(GOTO_FRAME);
        gen1(QUOTE_FRAME, label);
        gen1(EVAL_SEQUENTIAL_FRAME, o);
        return;
      default:
        pop_rewinding();
        break;
    }
  }
  set_fp(i);
  exit1();
}

static void pop_fetch_operator_frame(void)
{
  object args;
  int (*special)(int, object);
  args = get_frame_var(fp, 0);
  pop_frame();
  switch (object_type(reg[0])) {
    case SPECIAL:
      special = reg[0]->builtin.u.special;
      (*special)(object_list_len(args), args);
      return;
    case FUNCITON:
      gen1(APPLY_BUILTIN_FRAME, reg[0]);
      gen_eval_args_frame(args);
      return;
    case MACRO:
      gen0(EVAL_FRAME);
      gen1(APPLY_FRAME, reg[0]);
      reg[0] = args;
      return;
    case LAMBDA:
      gen1(APPLY_FRAME, reg[0]);
      gen_eval_args_frame(args);
      return;
    default: break;
  }
  ip_mark_error("is not a operator");
}

static void pop_eval_args_frame(void)
{
  object rest, acc;
  rest = get_frame_var(fp, 0);
  acc = gc_new_cons(reg[0], get_frame_var(fp, 1));
  set_frame_var(fp, 1, acc);
  if (rest == object_nil) {
    pop_frame();
    reg[0] = object_reverse(acc);
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
      case LAMBDA_FRAME:
        pop_lambda_frame();
        return;
      case UNWIND_PROTECT_FRAME: 
        args = get_frame_var(fp, 0);
        pop_frame();
        gen0(RETURN_FRAME);
        gen1(QUOTE_FRAME, reg[0]);
        gen1(EVAL_SEQUENTIAL_FRAME, args);
        return;
      default:
        pop_rewinding();
        break;
    }
  }
}

static object anonimous_lambda;
static object named_lambda;

static void find_named_lambda(void *key, void *data)
{
  if (anonimous_lambda == data) named_lambda = key;
}

static int resolve_anonimous_lambda(void)
{
  object e;
  xassert(named_lambda == NULL);
  e = reg[1];
  while (e != object_nil) {
    splay_foreach(&e->env.binding, find_named_lambda);
    if (named_lambda != NULL) return TRUE;
    e = e->env.top;
  }
  return FALSE;
}

static object call_stack(void)
{
  int i;
  object o;
  o = object_nil;
  for (i = 0; i <= fp; i = next_fp(i)) {
    switch (fs_nth(i)) {
      case LAMBDA_FRAME:
        named_lambda = NULL;
        anonimous_lambda = get_frame_var(i, 1);
        if (resolve_anonimous_lambda()) o = gc_new_cons(named_lambda, o);
        else o = gc_new_cons(anonimous_lambda, o);
        break;
      default:
        break;
    }
  }
  return o;
}

static int object_p(object o);
static int object_is_a_p(object o, object cls_sym);

static void push_call_stack(object o)
{
  while (object_type_p(o, CONS)) {
    if (o->cons.car == object_stack_trace) {
      if (object_type_p((o = o->cons.cdr), CONS)) {
        if (o->cons.car == object_nil) o->cons.car = call_stack();
      }
      return;
    }
    o = o->cons.cdr;
  }
}

static void pop_throw_frame(void)
{
  int i, j;
  object cls_sym, handler, handlers, body;
  i = fp;
  pop_frame();
  push_call_stack(reg[0]);
  while (fp > -1) {
    switch (fs_top()) {
      case UNWIND_PROTECT_FRAME:
        body = get_frame_var(fp, 0);
        pop_frame();
        gen0(THROW_FRAME);
        gen1(QUOTE_FRAME, reg[0]);
        gen1(EVAL_SEQUENTIAL_FRAME, body);
        return;
      case HANDLERS_FRAME:
        handlers = get_frame_var(fp, 0);
        xassert(object_type_p(handlers, ARRAY));
        pop_frame();
        for (j = 0; j < handlers->array.size; j += 2) {
          cls_sym = handlers->array.elt[j];
          handler = handlers->array.elt[j + 1];
          if (!object_is_a_p(reg[0], cls_sym)) continue;
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

// built in functions

DEFUN(eq_p)
{
  object o;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  o = argv->cons.car;
  reg[0] = object_nil;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (o != argv->cons.car) return TRUE;
  }
  reg[0] = object_true;
  return TRUE;
}

DEFUN(neq_p)
{
  object o;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  o = argv->cons.car;
  reg[0] = object_nil;
  while ((argv = argv->cons.cdr) != object_nil) {
    if (o == argv->cons.car) return TRUE;
  }
  reg[0] = object_true;
  return TRUE;
}

DEFUN(address)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  reg[0] = gc_new_xint((intptr_t)argv->cons.car);
  return TRUE;
}

DEFUN(not)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  reg[0] = object_bool(argv->cons.car == object_nil);
  return TRUE;
}

DEFUN(gensym)
{
  static int c = 0;
  xbarray_reset(&bi_buf);
  xbarray_addf(&bi_buf, "$G-%d", ++c);
  reg[0] = gc_new_bytes_from(SYMBOL, bi_buf.elt, bi_buf.size);
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
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  switch (object_type(argv->cons.car)) {
    case FUNCITON:
      gen1(APPLY_BUILTIN_FRAME, argv->cons.car);
      reg[0] = argv->cons.cdr->cons.car;
      return TRUE;
    case LAMBDA:
      gen1(APPLY_FRAME, argv->cons.car);
      reg[0] = argv->cons.cdr->cons.car;
      return TRUE;
    default: break;
  }
  ip_mark_error("requires function or symbol(built in function) to apply");
  return FALSE;
}

DEFUN(expand_macro)
{
  object f, args;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  reg[0] = argv->cons.car;
  if (!object_type_p(reg[0], CONS)) return TRUE;
  f = reg[0]->cons.car;
  args = reg[0]->cons.cdr;
  if (!object_type_p(f, SYMBOL)) return TRUE;
  if ((f = symbol_find_propagation(reg[1], f)) == NULL) return TRUE;
  if (!object_type_p(f, MACRO)) return TRUE;
  gen1(APPLY_FRAME, f);
  reg[0] = args;
  return TRUE;
}

DEFUN(bound_p)
{
  object s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!object_type_p((s = argv->cons.car), SYMBOL)) {
    ip_mark_error("required symbol");
    return FALSE;
  }
  reg[0] = object_bool(symbol_find_propagation(reg[1], s) != NULL);
  return TRUE;
}

static void trap(void)
{
  switch (ip_trap_code) {
    case TRAP_ERROR:
      xassert(error_msg != NULL);
      gen0(THROW_FRAME);
      reg[0] = gc_new_Error(error_msg);
      ip_trap_code = TRAP_NONE;
      error_msg = NULL;
      break;
    default:
      ip_trap_code = TRAP_NONE;
      break;
  }
}

// paren object system.

#define class_sym(cls) ((cls)->cons.cdr->cons.cdr->cons.cdr->cons.car)
#define class_super(cls) ((cls)->cons.cdr->cons.cdr->cons.cdr->cons.cdr \
    ->cons.cdr->cons.car)
#define class_features(cls) ((cls)->cons.cdr->cons.cdr->cons.cdr->cons.cdr \
    ->cons.cdr->cons.cdr->cons.cdr->cons.car)

static int object_class_p(object o)
{
  if (!object_type_p(o, CONS)) return FALSE;
  if (o->cons.car != object_class) return FALSE;
  if (!object_type_p(o = o->cons.cdr, CONS)) return FALSE;
  if (o->cons.car != object_Class) return FALSE;
  if (!object_type_p(o = o->cons.cdr, CONS)) return FALSE;
  if (o->cons.car != object_symbol) return FALSE;
  if (!object_type_p(o = o->cons.cdr, CONS)) return FALSE;
  if (!object_type_p(o = o->cons.cdr, CONS)) return FALSE;
  if (o->cons.car != object_super) return FALSE;
  if (!object_type_p(o = o->cons.cdr, CONS)) return FALSE;
  if (!object_type_p(o = o->cons.cdr, CONS)) return FALSE;
  if (o->cons.car != object_features) return FALSE;
  if (!object_type_p(o = o->cons.cdr, CONS)) return FALSE;
  if (!object_type_p(o = o->cons.cdr, CONS)) return FALSE;
  if (o->cons.car != object_fields) return FALSE;
  if (!object_type_p(o = o->cons.cdr, CONS)) return FALSE;
  return o->cons.cdr == object_nil;
}

static int find_class(object cls_sym, object *result)
{
  if (!object_type_p(cls_sym, SYMBOL)) return FALSE;
  if ((*result = symbol_find_propagation(reg[1], cls_sym)) == NULL) return FALSE;
  return object_class_p(*result);
}

static int find_super_class(object cls_sym, object *result)
{
  object cls;
  if (!find_class(cls_sym, &cls)) return FALSE;
  return find_class(class_super(cls), result);
}

static int find_class_method(object cls_sym, object mtd_sym, object *result)
{
  object s;
  xassert(object_type_p(cls_sym, SYMBOL));
  xassert(object_type_p(mtd_sym, SYMBOL));
  xbarray_reset(&bi_buf);
  xbarray_copy(&bi_buf, cls_sym->bytes.elt, cls_sym->bytes.size);
  xbarray_copy(&bi_buf, mtd_sym->bytes.elt, mtd_sym->bytes.size);
  s = gc_new_bytes_from(SYMBOL, bi_buf.elt, bi_buf.size);
  if (((*result) = symbol_find_propagation(reg[1], s)) == NULL) return TRUE;
  if (!object_type_p(*result, LAMBDA)) {
    ip_mark_error("is not a method");
    return FALSE;
  }
  return TRUE;
}

static int object_p(object o)
{
  if (!object_type_p(o, CONS)) return FALSE;
  if (o->cons.car != object_class) return FALSE;
  o = o->cons.cdr;
  while (TRUE) {
    if (!object_type_p(o, CONS)) return FALSE;
    if (o->cons.cdr == object_nil) return TRUE;
    o = o->cons.cdr;
    if (!object_type_p(o, CONS)) return FALSE;
    if (!object_type_p(o->cons.car, KEYWORD)) return FALSE;
    o = o->cons.cdr;
  }
}

static int object_is_a_p(object o, object cls_sym) {
  object o_cls_sym;
  xassert(object_type_p(cls_sym, SYMBOL));
  if (!object_p(o)) return FALSE;
  o_cls_sym = o->cons.cdr->cons.car;
  while (TRUE) {
    if (o_cls_sym == cls_sym) return TRUE;
    if (!find_super_class(o_cls_sym, &o)) return FALSE;
    o_cls_sym = class_sym(o);
  }
}

DEFUN(object_p)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  reg[0] = object_bool(object_p(argv->cons.car));
  return TRUE;
}

DEFUN(is_a_p)
{
  object o, cls;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  o = argv->cons.car;
  if (!object_class_p(cls = argv->cons.cdr->cons.car)) {
    ip_mark_error("require Class instance");
    return FALSE;
  }
  reg[0] = object_bool(object_is_a_p(o, class_sym(cls)));
  return TRUE;
}

DEFUN(find_class)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!find_class(argv->cons.car, result)) {
    ip_mark_error("class not found");
    return FALSE;
  }
  return TRUE;
}

DEFUN(find_method)
{
  object cls, cls_sym, mtd_sym, features;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &cls_sym)) return FALSE;
  argv = argv->cons.cdr;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &mtd_sym)) return FALSE;
  while (TRUE) {
    // find class mtehod
    if (!find_class_method(cls_sym, mtd_sym, result)) return FALSE;
    if (*result != NULL) return TRUE;
    // find feature method
    if (!find_class(cls_sym, &cls)) return FALSE;
    features = class_features(cls);
    while (features != object_nil) {
      xassert(object_type_p(features, CONS));
      if (!find_class_method(features->cons.car, mtd_sym, result))
        return FALSE;
      if (*result != NULL) return TRUE;
      features = features->cons.cdr;
    }
    // super class
    if (!find_class(class_super(cls), &cls)) {
      ip_mark_error("method not found");
      return FALSE;
    }
    cls_sym = class_sym(cls);
  }
}

DEFUN(stack_frame)
{
  dump_fs();
  return TRUE;
}

// special operator

static int parse_rest_param(object params)
{
  switch (object_type(params->cons.car)) {
    case SYMBOL:
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
        break;
      default:
        return FALSE;
    }
  }
  return TRUE;
}

static int parse_required_params(object params, int nest_p)
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
        if (!nest_p) return FALSE;
        if (!parse_required_params(params->cons.car, nest_p)) return FALSE;
        params = params->cons.cdr;
        break;
      case SYMBOL:
        params = params->cons.cdr;
        break;
      default:
        return FALSE;
    }
  }
  return TRUE;
}

static int gen_symbol_binding(object args, int frame_type)
{
  object o;
  if (args == object_nil) return TRUE;
  if (!object_type_p((o = args)->cons.car, SYMBOL)) {
    ip_mark_error("argument must be symbol");
    return FALSE;
  }
  if ((args = args->cons.cdr) == object_nil) {
    ip_mark_error("argument must be pairs");
    return FALSE;
  }
  if (!gen_symbol_binding(args->cons.cdr, frame_type)) return FALSE;
  gen1(frame_type, o->cons.car);
  gen0(EVAL_FRAME);
  gen1(QUOTE_FRAME, o->cons.cdr->cons.car);
  return TRUE;
}

DEFSP(let)
{
  object args;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &args)) return FALSE;
  gen1(LET_FRAME, reg[1]);
  gen1(EVAL_SEQUENTIAL_FRAME, argv->cons.cdr);
  reg[1] = gc_new_env(reg[1]);
  return gen_symbol_binding(args, BIND_FRAME);
}

DEFSP(dynamic)
{
  int i;
  object e, s;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &s)) return FALSE;
  i = fp;
  e = reg[1];
  if ((reg[0] = symbol_find(e, s)) != NULL) return TRUE;
  while ((i = prev_fp(i)) != -1) {
    switch (fs_nth(i)) {
      case LET_FRAME:
        e = e->env.top;
        break;
      case LAMBDA_FRAME:
        e = get_frame_var(i, 0);
        break;
      default:
        continue;
    }
    if ((reg[0] = symbol_find(e, s)) != NULL) return TRUE;
  }
  return FALSE;
}

DEFSP(symbol_bind)
{
  return gen_symbol_binding(argv, BIND_PROPAGATION_FRAME);
}

DEFSP(begin)
{
  gen1(EVAL_SEQUENTIAL_FRAME, argv);
  return TRUE;
}

DEFSP(macro)
{
  object o, params;
  if (!bi_argc_range(argc, 3, FALSE)) return FALSE;
  if (!bi_arg_type(argv->cons.car, SYMBOL, &o)) return FALSE;
  gen1(BIND_PROPAGATION_FRAME, o);
  argv = argv->cons.cdr;
  if (!parse_required_params(params = argv->cons.car, TRUE)) {
    mark_illegal_args();
    return FALSE;
  }
  reg[0] = gc_new_macro(reg[1], params, argv->cons.cdr);
  return TRUE;
}

DEFSP(lambda)
{
  object params;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  params = argv->cons.car;
  if (!parse_required_params(params, FALSE)) {
    mark_illegal_args();
    return FALSE;
  }
  reg[0] = gc_new_lambda(reg[1], params, argv->cons.cdr);
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

DEFSP(unwind_protect)
{
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  gen1(UNWIND_PROTECT_FRAME, argv->cons.cdr);
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

DEFSP(labels)
{
  gen1(LABELS_FRAME, argv);
  gen1(EVAL_SEQUENTIAL_FRAME, argv);
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
  if (!object_is_a_p(reg[0], object_Exception)) {
    ip_mark_error("must be Exception object");
    return FALSE;
  }
  gen0(THROW_FRAME);
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

DEFSP(catch)
{
  object params;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  if (!bi_arg_list(argv->cons.car, &params)) return FALSE;
  gen1(HANDLERS_FRAME, gc_new_array(object_list_len(params)));
  gen1(EVAL_SEQUENTIAL_FRAME, argv->cons.cdr);
  return gen_symbol_binding(params, BIND_HANDLER_FRAME);
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
  gen0(ASSERT_FRAME);
  gen_eval_frame(argv->cons.car);
#endif
  return TRUE;
}

// main

static void ip_main(object args)
{
  reg[0] = object_nil;
  reg[1] = object_toplevel;
  gen1(EVAL_SEQUENTIAL_FRAME, args);
  while (fp != -1) {
    xassert(fp >= 0);
    if (ip_trap_code != TRAP_NONE) trap();
    if (cycle % IP_POLLING_INTERVAL == 0) {
      gc_chance();
    }
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
      case FETCH_OPERATOR_FRAME: pop_fetch_operator_frame(); break;
      case GOTO_FRAME: pop_goto_frame(); break;
      case HANDLERS_FRAME: pop_frame(); break;
      case IF_FRAME: pop_if_frame(); break;
      case LABELS_FRAME: pop_frame(); break;
      case LAMBDA_FRAME: pop_lambda_frame(); break;
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
}

void ip_start(object args)
{
  sp = 0;
  fp = -1;
  cycle = 0;
  xbarray_init(&bi_buf);
  ip_trap_code = TRAP_NONE;
  ip_main(args);
}
