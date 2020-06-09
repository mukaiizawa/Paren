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
static struct xarray fb;

// frame_type
#define FRAME_SIZE_MASK          0x0000000f
#define   APPLY_FRAME            0x00000003
#define   APPLY_BUILTIN_FRAME    0x00000013
#define   ASSERT_FRAME           0x00000022
#define   BIND_HANDLER_FRAME     0x00000034
#define   BIND_FRAME             0x00000043
#define   BIND_PROPAGATION_FRAME 0x00000053
#define   EVAL_FRAME             0x00000062
#define   EVAL_ARGS_FRAME        0x00000074
#define   EVAL_SEQUENTIAL_FRAME  0x00000083
#define   FENCE_FRAME            0x00000092
#define   FETCH_OPERATOR_FRAME   0x00000103
#define   GOTO_FRAME             0x00000112
#define   HANDLERS_FRAME         0x00000123
#define   IF_FRAME               0x00000133
#define   LABELS_FRAME           0x00000143
#define   QUOTE_FRAME            0x00000153
#define   RETURN_FRAME           0x00000162
#define   SWITCH_ENV_FRAME       0x00000173
#define   THROW_FRAME            0x00000182
#define   TRACE_FRAME            0x00000193
#define   UNWIND_PROTECT_FRAME   0x00000203

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
    case FENCE_FRAME: return "FENCE_FRAME";
    case FETCH_OPERATOR_FRAME: return "FETCH_OPERATOR_FRAME";
    case GOTO_FRAME: return "GOTO_FRAME";
    case HANDLERS_FRAME: return "HANDLERS_FRAME";
    case IF_FRAME: return "IF_FRAME";
    case LABELS_FRAME: return "LABELS_FRAME";
    case QUOTE_FRAME: return "QUOTE_FRAME";
    case RETURN_FRAME: return "RETURN_FRAME";
    case SWITCH_ENV_FRAME: return "SWITCH_ENV_FRAME";
    case THROW_FRAME: return "THROW_FRAME";
    case TRACE_FRAME: return "TRACE_FRAME";
    case UNWIND_PROTECT_FRAME: return "UNWIND_PROTECT_FRAME";
    default: xassert(FALSE); return NULL;
  }
}

#define fs_top() (sint_val(fs[fp]))
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
#define fb_gen0(frame_type) xarray_add(&fb, sint(frame_type));
#define fb_gen1(frame_type, o) \
{ \
  xarray_add(&fb, o); \
  fb_gen0(frame_type); \
}
#define fb_gen2(frame_type, o, p) \
{ \
  xarray_add(&fb, p); \
  fb_gen1(frame_type, o); \
}

static void gen(int frame_type)
{
  if (sp > FRAME_STACK_SIZE - STACK_GAP) ip_mark_error("stack over flow");
  fs[sp + 1] = sint(fp);
  fs[sp] = sint(frame_type);
  fp = sp;
  sp = next_fp(fp);
}

static void pop_frame(void)
{
  sp = fp;
  fp = prev_fp(fp);
}

// frame buffer

static void fb_reset(void)
{
  xarray_reset(&fb);
}

static void fb_flush(void)
{
  int i;
  object o;
  i = fb.size - 1;
  while (i > 0) {
    o = fb.elt[i--];
    gen(sint_val(o));
    switch (frame_size(sint_val(o))) {
      case 2:    // fb_gen0
        break;
      case 3:    // fb_gen1
        set_frame_var(fp, 0, fb.elt[i--]);
        break;
      case 4:    // fb_gen2
        set_frame_var(fp, 0, fb.elt[i--]);
        set_frame_var(fp, 1, fb.elt[i--]);
        break;
      default: xassert(FALSE); break;
    }
  }
}

static void gen_apply_frame(object operator)
{
  gen0(FENCE_FRAME);
  gen1(APPLY_FRAME, operator);
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

static void gen_switch_env_frame(object env)
{
  gen1(SWITCH_ENV_FRAME, reg[1]);
  reg[1] = gc_new_env(env);
}

static int same_symbol_keyword_p(object sym, object key)
{
  xassert(object_type_p(sym, SYMBOL) && object_type_p(key, KEYWORD));
  if (sym->barray.size != key->barray.size) return FALSE;
  return memcmp(sym->barray.elt, key->barray.elt, sym->barray.size) == 0;
}

static int valid_keyword_p(object params, object args)
{
  object p, s;
  while (args != object_nil) {
    if (!object_type_p(args->cons.car, KEYWORD)) {
      ip_mark_error("expected keyword parameter");
      return FALSE;
    }
    p = params;
    while (p != object_nil) {
      s = p->cons.car;
      if (object_type_p(s, CONS)) s = s->cons.car;
      xassert(object_type_p(s, SYMBOL));
      if (same_symbol_keyword_p(s, args->cons.car)) break;
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

static void parse_args(object env, object params, object args)
{
  object o, pre, k, v, def_v, sup_k;
  // parse required args
  while (params != object_nil && !object_type_p(params->cons.car, KEYWORD)) {
    if (args == object_nil) {
      bi_argc_range(0, 1, 1);
      return;
    }
    if (!object_type_p(params->cons.car, CONS)) {
      fb_gen1(QUOTE_FRAME, args->cons.car);
      fb_gen1(BIND_FRAME, params->cons.car);
    } else if (!object_list_p(args->cons.car)) {
      mark_illegal_args();
      return;
    } else {
      parse_args(env, params->cons.car, args->cons.car);
      if (ip_trap_code != TRAP_NONE) return;
    }
    params = params->cons.cdr;
    args = args->cons.cdr;
  }
  // parse optional args
  if (params->cons.car == object_opt) {
    params = params->cons.cdr;
    while (params != object_nil && !object_type_p(params->cons.car, KEYWORD)) {
      o = params->cons.car;
      def_v = sup_k = NULL;
      if (!object_type_p(o, CONS)) k = o;
      else {
        k = o->cons.car;
        def_v = (o = o->cons.cdr)->cons.car;
        if ((o = o->cons.cdr) != object_nil) sup_k = o->cons.car;
      }
      params = params->cons.cdr;
      if (sup_k != NULL) {
        v = object_bool(args != object_nil);
        fb_gen1(QUOTE_FRAME, v);
        fb_gen1(BIND_FRAME, sup_k);
      }
      if (args != object_nil) {
        fb_gen1(QUOTE_FRAME, args->cons.car);
        fb_gen1(BIND_FRAME, k);
        args = args->cons.cdr;
      } else {
        if (def_v == NULL) {
          fb_gen1(QUOTE_FRAME, object_nil);
          fb_gen1(BIND_FRAME, k);
        } else {
          fb_gen1(QUOTE_FRAME, def_v);
          fb_gen0(EVAL_FRAME);
          fb_gen1(BIND_FRAME, k);
        }
      }
    }
  }
  // parse rest args
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    fb_gen1(QUOTE_FRAME, args);
    fb_gen1(BIND_FRAME, params->cons.car);
    return;
  }
  // parse keyword args
  if (params->cons.car == object_key) {
    params = params->cons.cdr;
    if (!valid_keyword_p(params, args)) return;
    while (params != object_nil) {
      o = params->cons.car;
      v = def_v = sup_k = NULL;
      if (!object_type_p(o, CONS)) k = o;
      else {
        k = o->cons.car;
        def_v = (o = o->cons.cdr)->cons.car;
        if ((o = o->cons.cdr) != object_nil) sup_k = o->cons.car;
      }
      o = args;
      pre = NULL;
      while (o != object_nil) {
        if (same_symbol_keyword_p(k, o->cons.car)) {
          v = (o = o->cons.cdr)->cons.car;
          o = o->cons.cdr;
          if (pre == NULL) args = o;
          else pre->cons.cdr = o;
          break;
        }
        pre = o = o->cons.cdr;
        o = o->cons.cdr;
      }
      if (sup_k != NULL) {
        fb_gen1(QUOTE_FRAME, object_bool(v != NULL));
        fb_gen1(BIND_FRAME, sup_k);
      }
      if (v != NULL) {
        fb_gen1(QUOTE_FRAME, v);
        fb_gen1(BIND_FRAME, k);
      }
      else {
        if (def_v == NULL) {
          fb_gen1(QUOTE_FRAME, object_nil);
          fb_gen1(BIND_FRAME, k);
        } else {
          fb_gen1(QUOTE_FRAME, def_v);
          fb_gen0(EVAL_FRAME);
          fb_gen1(BIND_FRAME, k);
        }
      }
      params = params->cons.cdr;
    }
  }
  if (args != object_nil) bi_argc_range(2, 1, 1);
}

static void pop_apply_frame(void)
{
  object operator;
  operator = get_frame_var(fp, 0);
  pop_frame();
  fb_reset();
  gen_switch_env_frame(operator->lambda.env);
  parse_args(reg[1], operator->lambda.params, reg[0]);
  gen_eval_sequential_frame(operator->lambda.body);
  fb_flush();
}

static void pop_builtin_inst(void)
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
  int i, j;
  object cls_sym, handler, handlers;
  handler = reg[0];
  i = sint_val(get_frame_var(fp, 0));
  cls_sym = get_frame_var(fp, 1);
  pop_frame();
  if (!object_type_p(handler, LAMBDA)) {
    ip_mark_error("required handler");
    return;
  }
  for (j = fp; j > -1; j = prev_fp(j)) {
    if (sint_val(fs[j]) == HANDLERS_FRAME) {
      handlers = get_frame_var(j, 0);
      xassert(i + 1 < handlers->array.size);
      handlers->array.elt[i] = cls_sym;
      handlers->array.elt[i + 1] = handler;
      return;
    }
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
        gen1(TRACE_FRAME, reg[0]);
        ip_mark_error("unbind symbol");
        return;
      }
      reg[0] = s;
      return;
    case CONS:
      gen1(TRACE_FRAME, reg[0]);
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
    case BARRAY:
    case ARRAY:
      return;
    default:
      xassert(FALSE);
      return;
  }
}

static void pop_swith_env_frame(void)
{
  reg[1] = get_frame_var(fp, 0);
  pop_frame();
}

static void pop_rewinding(void)
{
  switch (fs_top()) {
    case SWITCH_ENV_FRAME: pop_swith_env_frame(); break;
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
  object o, label;
  label = reg[0];
  if (!object_type_p(label, KEYWORD)) {
    ip_mark_error("label must be keyword");
    return;
  }
  while (TRUE) {
    if (fp < 0) {
      ip_mark_error("labels context not found");
      return;
    }
    if (fs_top() == UNWIND_PROTECT_FRAME) {
      o = get_frame_var(fp, 0);
      pop_frame();
      gen0(GOTO_FRAME);
      gen1(QUOTE_FRAME, label);
      gen_eval_sequential_frame(o);
      return;
    }
    if (fs_top() == LABELS_FRAME) break;
    pop_rewinding();
  }
  o = get_frame_var(fp, 0);
  while (TRUE) {
    if (o == object_nil) {
      ip_mark_error("label not found");
      return;
    }
    if (o->cons.car == label) break;
    o = o->cons.cdr;
  }
  gen_eval_sequential_frame(o);
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
      gen_apply_frame(reg[0]);
      reg[0] = args;
      return;
    case LAMBDA:
      gen_apply_frame(reg[0]);
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
  if (args == object_nil) pop_frame();
  else {
    set_frame_var(fp, 0, args->cons.cdr);
    gen_eval_frame(args->cons.car);
  }
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
  while (sp > 0) {
    switch (fs_top()) {
      case FENCE_FRAME:
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

static object call_stack(void)
{
  int i;
  object o;
  o = object_nil;
  for (i = 0; i <= fp; i = next_fp(i)) {
    if (sint_val(fs[i]) == TRACE_FRAME)
      o = gc_new_cons(get_frame_var(i, 0), o);
  }
  return o;
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
  exit(1);
}

static int object_p(object o);
static int object_is_a_p(object e, object o, object cls_sym);

static void push_call_stack(object o)
{
  while (object_type_p(o, CONS)) {
    if (o->cons.car == object_stack_trace) {
      if (object_type_p((o = o->cons.cdr), CONS) && o->cons.car == object_nil)
        o->cons.car = call_stack();
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
  if (!object_is_a_p(reg[1], reg[0], object_Exception))
    reg[0] = gc_new_Error("must be Exception object");
  push_call_stack(reg[0]);
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
          if (!object_is_a_p(reg[1], reg[0], cls_sym)) continue;
          gen_apply_frame(handler);
          reg[0] = gc_new_cons(reg[0], object_nil);
          return;
        }
        break;
      default:
        pop_rewinding();
        break;
    }
  }
  fp = i;
  exit1();
}

// built in functions

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
      gen_apply_frame(argv->cons.car);
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
  gen_apply_frame(f);
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

static int find_class(object e, object cls_sym, object *result)
{
  if (!object_type_p(cls_sym, SYMBOL)) return FALSE;
  if ((*result = symbol_find_propagation(e, cls_sym)) == NULL) return FALSE;
  return object_class_p(*result);
}

static int find_super_class(object e, object cls_sym, object *result)
{
  object cls;
  if (!find_class(e, cls_sym, &cls)) return FALSE;
  return find_class(e, class_super(cls), result);
}

static int find_class_method(object e, object cls_sym, object mtd_sym
    , object *result)
{
  struct xbarray x;
  object s;
  xassert(object_type_p(cls_sym, SYMBOL));
  xassert(object_type_p(mtd_sym, SYMBOL));
  xbarray_init(&x);
  xbarray_copy(&x, cls_sym->barray.elt, cls_sym->barray.size);
  xbarray_copy(&x, mtd_sym->barray.elt, mtd_sym->barray.size);
  s = gc_new_barray_from(SYMBOL, x.elt, x.size);
  xbarray_free(&x);
  if (((*result) = symbol_find_propagation(e, s)) == NULL) return TRUE;
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

static int object_is_a_p(object e, object o, object cls_sym) {
  object o_cls_sym;
  xassert(object_type_p(cls_sym, SYMBOL));
  if (!object_p(o)) return FALSE;
  o_cls_sym = o->cons.cdr->cons.car;
  while (TRUE) {
    if (o_cls_sym == cls_sym) return TRUE;
    if (!find_super_class(e, o_cls_sym, &o)) return FALSE;
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
  reg[0] = object_bool(object_is_a_p(reg[1], o, class_sym(cls)));
  return TRUE;
}

DEFUN(find_class)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!find_class(reg[1], argv->cons.car, result)) {
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
    if (!find_class_method(reg[1], cls_sym, mtd_sym, result)) return FALSE;
    if (*result != NULL) return TRUE;
    // find feature method
    if (!find_class(reg[1], cls_sym, &cls)) return FALSE;
    features = class_features(cls);
    while (features != object_nil) {
      xassert(object_type_p(features, CONS));
      if (!find_class_method(reg[1], features->cons.car, mtd_sym, result))
        return FALSE;
      if (*result != NULL) return TRUE;
      features = features->cons.cdr;
    }
    // super class
    if (!find_class(reg[1], class_super(cls), &cls)) {
      ip_mark_error("method not found");
      return FALSE;
    }
    cls_sym = class_sym(cls);
  }
}

DEFUN(stack_frame)
{
  int i;
  char buf[MAX_STR_LEN];
  for (i = 0; i <= fp; i = next_fp(i)) {
    printf("+-----------------------------\n");
    printf("|%d: %s\n", i, frame_name(sint_val(fs[i])));
    switch (sint_val(fs[i])) {
      case HANDLERS_FRAME:
      case TRACE_FRAME:
        printf("|%s\n", object_describe(get_frame_var(i, 0), buf));
        break;
      default:
        break;
    }
  }
  exit(1);
}

// special operator

/*
 * <lambda_list> ::= [<required_params>] 
 *                   [:opt <xparams>]
 *                   { [:rest <param>] | [:key <xparams>] }
 * <required_params> ::= <param> <param> ...
 * <xparams> ::= <xparam> <xparam> ...
 * <xparam> ::= { <param> | (<param> <initial_value> [<supplyp>]) }
 */
static int valid_xparam_p(object o)
{
  o = o->cons.car;
  if (object_type_p(o, SYMBOL)) return TRUE;
  return object_type_p(o, CONS) && object_type_p(o->cons.car, SYMBOL)
    && object_type_p(o = o->cons.cdr, CONS)
    && (o->cons.cdr == object_nil
        || (object_type_p(o = o->cons.cdr, CONS)
          && object_type_p(o->cons.car, SYMBOL) && o->cons.cdr == object_nil));
}

static int parse_params(object *o)
{
  *o = (*o)->cons.cdr;
  if (!valid_xparam_p(*o)) return FALSE;
  *o = (*o)->cons.cdr;
  while (TRUE) {
    if (*o == object_nil) break;
    if (object_type_p((*o)->cons.car, KEYWORD)) break;
    if (!valid_xparam_p(*o)) return FALSE;
    *o = (*o)->cons.cdr;
  }
  return TRUE;
}

static int valid_lambda_list_p(object params, int nest_p)
{
  int type;
  while (TRUE) {
    if (!object_list_p(params)) return FALSE;
    if (params == object_nil) return TRUE;
    type = object_type(params->cons.car);
    if (type == KEYWORD) break;
    else if (type == SYMBOL) params = params->cons.cdr;
    else if (type == CONS) {
      if (!nest_p) return FALSE;
      if (!valid_lambda_list_p(params->cons.car, nest_p)) return FALSE;
      params = params->cons.cdr;
    }
    else return FALSE;
  }
  if (params->cons.car == object_opt)
    if (!parse_params(&params)) return FALSE;
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    if (!object_type_p(params->cons.car, SYMBOL)) return FALSE;
    params = params->cons.cdr;
  } else if (params->cons.car == object_key) {
    if (!parse_params(&params)) return FALSE;
  }
  return params == object_nil;
}

static int gen_let_binding(int i, object args)
{
  object o;
  if (args == object_nil) return TRUE;
  if (!object_type_p(args->cons.car, SYMBOL)) {
    ip_mark_error("argument must be symbol");
    return FALSE;
  }
  if ((o = args->cons.cdr) == object_nil) {
    ip_mark_error("argument must be pairs");
    return FALSE;
  }
  if (!gen_let_binding(i + 2, o->cons.cdr)) return FALSE;
  gen1(BIND_FRAME, args->cons.car);
  gen0(EVAL_FRAME);
  gen1(QUOTE_FRAME, args->cons.cdr->cons.car);
  return TRUE;
}

DEFSP(let)
{
  object args;
  if (!bi_argc_range(argc, 1, FALSE)) return FALSE;
  if (!object_list_p((args = argv->cons.car))) {
    ip_mark_error("argument must be list");
    return FALSE;
  }
  gen_switch_env_frame(reg[1]);
  gen_eval_sequential_frame(argv->cons.cdr);
  return gen_let_binding(0, args);
}

DEFSP(dynamic)
{
  int i;
  object e, s, v;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!object_type_p((s = argv->cons.car), SYMBOL)) {
    ip_mark_error("argument must be symbol");
    return FALSE;
  }
  i = fp;
  e = reg[1];
  while (TRUE) {
    if ((v = symbol_find(e, s)) != NULL) break;
    while ((i = prev_fp(i)) > 0) {
      if (sint_val(fs[i]) == SWITCH_ENV_FRAME) {
        e = get_frame_var(i, 0);
        break;
      }
    }
    if (i < 0) {
      ip_mark_error("unbind symbol");
      return FALSE;
    }
  }
  reg[0] = v;
  return TRUE;
}

DEFSP(symbol_bind)
{
  object s;
  if (argc == 0) return TRUE;
  if (argc % 2 != 0) {
    ip_mark_error("argument must be pair");
    return FALSE;
  }
  fb_reset();
  while (argc != 0) {
    s = argv->cons.car;
    if (!object_type_p(s, SYMBOL)) {
      ip_mark_error("cannot bind except symbol");
      return FALSE;
    }
    if (s == object_nil) {
      ip_mark_error("cannot bind nil");
      return FALSE;
    }
    argv = argv->cons.cdr;
    fb_gen1(QUOTE_FRAME, argv->cons.car);
    fb_gen0(EVAL_FRAME);
    fb_gen1(BIND_PROPAGATION_FRAME, s);
    argv = argv->cons.cdr;
    argc -= 2;
  }
  fb_flush();
  return TRUE;
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
  if (!valid_lambda_list_p(params = argv->cons.car, TRUE)) {
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
  if (!valid_lambda_list_p(params, FALSE)) {
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
  gen_eval_sequential_frame(argv);
  return TRUE;
}

DEFSP(goto)
{
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  gen0(GOTO_FRAME);
  reg[0] = argv->cons.car;
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
  int size;
  object cls, params;
  if (!bi_argc_range(argc, 2, FALSE)) return FALSE;
  size = 0;
  params = argv->cons.car;
  fb_reset();
  while (params != object_nil) {
    if (!object_type_p(params, CONS)) {
      mark_illegal_args();
      return FALSE;
    }
    if (!object_type_p((cls = params->cons.car), SYMBOL)) {
      mark_illegal_args();
      return FALSE;
    }
    if (!object_type_p((params = params->cons.cdr), CONS)) {
      ip_mark_error("parameter must be pair");
      return FALSE;
    }
    fb_gen1(QUOTE_FRAME, params->cons.car);
    fb_gen0(EVAL_FRAME);
    fb_gen2(BIND_HANDLER_FRAME, sint(size), cls);
    params = params->cons.cdr;
    size += 2;
  }
  gen1(HANDLERS_FRAME, gc_new_array(size));
  gen_eval_sequential_frame(argv->cons.cdr);
  fb_flush();
  return TRUE;
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

static void ip_main(void)
{
  reg[0] = object_nil;
  reg[1] = object_toplevel;
  gen_apply_frame(object_boot);
  while (fp != -1) {
    xassert(fp >= 0);
    if (ip_trap_code != TRAP_NONE) trap();
    if (cycle % IP_POLLING_INTERVAL == 0) {
      gc_chance();
    }
    switch (fs_top()) {
      case APPLY_FRAME: pop_apply_frame(); break;
      case APPLY_BUILTIN_FRAME: pop_builtin_inst(); break;
      case ASSERT_FRAME: pop_assert_frame(); break;
      case BIND_FRAME: pop_bind_frame(); break;
      case BIND_HANDLER_FRAME: pop_bind_handler_frame(); break;
      case BIND_PROPAGATION_FRAME: pop_bind_propagation_frame(); break;
      case EVAL_FRAME: pop_eval_frame(); break;
      case EVAL_ARGS_FRAME: pop_eval_args_frame(); break;
      case EVAL_SEQUENTIAL_FRAME: pop_eval_sequential_frame(); break;
      case FENCE_FRAME: pop_frame(); break;
      case FETCH_OPERATOR_FRAME: pop_fetch_operator_frame(); break;
      case GOTO_FRAME: pop_goto_frame(); break;
      case HANDLERS_FRAME: pop_frame(); break;
      case IF_FRAME: pop_if_frame(); break;
      case LABELS_FRAME: pop_frame(); break;
      case QUOTE_FRAME: reg[0] = get_frame_var(fp, 0); pop_frame(); break;
      case RETURN_FRAME: pop_return_frame(); break;
      case SWITCH_ENV_FRAME: pop_swith_env_frame(); break;
      case THROW_FRAME: pop_throw_frame(); break;
      case TRACE_FRAME: pop_frame(); break;
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
  for (i = 0; i < sp; i++) {
    if (!sint_p(fs[i])) gc_mark(fs[i]);
  }
}

void ip_start(void)
{
  sp = 0;
  fp = -1;
  cycle = 0;
  xarray_init(&fb);
  ip_trap_code = TRAP_NONE;
  ip_main();
}
