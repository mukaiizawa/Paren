// interpreter

#include "std.h"
#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "splay.h"
#include "ip.h"

/*
 * registers:
 *   0 -- instruction argument.
 *   1 -- current environment.
 *   2 -- stack trace.
 *   3 -- where the exception occurred.
 */
#define REG_SIZE 4
static object reg[REG_SIZE];

static long cycle;

// error

int ip_trap_code;
static char *error_msg;

void ip_mark_exception(char *msg)
{
  ip_trap_code = TRAP_EXCEPTION;
  error_msg = msg;
}

void ip_mark_error(char *msg)
{
  ip_trap_code = TRAP_ERROR;
  error_msg = msg;
}

STATIC void ip_mark_too_few_arguments_error(void)
{
  ip_mark_exception("too few arguments");
}

STATIC void ip_mark_too_many_arguments_error(void)
{
  ip_mark_exception("too many arguments");
}

STATIC int ip_ensure_no_args(int argc)
{
  if (argc == 0) return TRUE;
  ip_mark_too_many_arguments_error();
  return FALSE;
}

int ip_ensure_arguments(int argc, int min, int max)
{
  if (argc < min) ip_mark_too_few_arguments_error();
  else if ((!min && !max && argc != 0) || (max && argc > max))
    ip_mark_too_many_arguments_error();
  else return TRUE;
  return FALSE;
}

void ip_mark_illegal_type(void)
{
  ip_mark_exception("illegal argument type");
}

int ip_ensure_type(int type, object o, object *result)
{
  if (!typep(o, type)) {
    ip_mark_illegal_type();
    return FALSE;
  }
  *result = o;
  return TRUE;
}

STATIC void mark_illegal_args_exception()
{
  ip_mark_exception("illegal parameter list");
}

// symbol

STATIC object symbol_find(object e, object s)
{
  xassert(typep(e, ENV));
  if (s == object_nil) return object_nil;
  return splay_find(e->env.binding, s);
}

STATIC object symbol_find_propagation(object e, object s)
{
  object v;
  while (e != object_nil) {
    if ((v = symbol_find(e, s)) != NULL) return v;
    e = e->env.top;
  }
  return NULL;
}

STATIC void symbol_bind(object e, object s, object v)
{
  xassert(typep(e, ENV) && typep(s, SYMBOL));
  splay_replace(e->env.binding, s, v);
}

STATIC void symbol_bind_propagation(object e, object s, object v)
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

// instructions

#define STACK_GAP 10
#define FRAME_STACK_SIZE 10000
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
 * | 2: frame_var 0  |
 * | 1: return addr  |
 * | 0: instruction  | <- ip
 * +-----------------+
 * |       ...       |
 * | 3: frame_var[1] |
 * | 2: frame_var[0] |
 * | 1: return addr  |
 * | 0: instruction  |
 * +-----------------+
 * |       ...       |
 * |       ...       |
 * |       ...       |
 * +-----------------+
 * |       ...       |
 * | 3: frame_var[1] |
 * | 2: frame_var[0] |
 * | 1: return addr  |
 * | 0: instruction  |
 * +-----------------+
 *
 */

static int ip;
static int sp;
static object fs[FRAME_STACK_SIZE];
static struct xarray fb;

// instructions
#define INST_SIZE_MASK          0x0000000f
#define   APPLY_INST            0x00000003
#define   APPLY_PRIM_INST       0x00000013
#define   ASSERT_INST           0x00000022
#define   BIND_INST             0x00000033
#define   BIND_PROPAGATION_INST 0x00000043
#define   EVAL_INST             0x00000052
#define   EVAL_ARGS_INST        0x00000064
#define   EVAL_SEQUENTIAL_INST  0x00000073
#define   FENCE_INST            0x00000082
#define   FETCH_HANDLER_INST    0x00000093
#define   FETCH_OPERATOR_INST   0x00000103
#define   GOTO_INST             0x00000112
#define   HANDLER_INST          0x00000123
#define   IF_INST               0x00000133
#define   LABELS_INST           0x00000143
#define   QUOTE_INST            0x00000153
#define   RETURN_INST           0x00000162
#define   SWITCH_ENV_INST       0x00000173
#define   THROW_INST            0x00000182
#define   TRACE_INST            0x00000193
#define   UNWIND_PROTECT_INST   0x00000203


char *inst_name(int inst)
{
  switch (inst) {
    case APPLY_INST: return "APPLY_INST";
    case APPLY_PRIM_INST: return "APPLY_PRIM_INST";
    case ASSERT_INST: return "ASSERT_INST";
    case BIND_INST: return "BIND_INST";
    case BIND_PROPAGATION_INST: return "BIND_PROPAGATION_INST";
    case EVAL_INST: return "EVAL_INST";
    case EVAL_ARGS_INST: return "EVAL_ARGS_INST";
    case EVAL_SEQUENTIAL_INST: return "EVAL_SEQUENTIAL_INST";
    case FENCE_INST: return "FENCE_INST";
    case FETCH_HANDLER_INST: return "FETCH_HANDLER_INST";
    case FETCH_OPERATOR_INST: return "FETCH_OPERATOR_INST";
    case GOTO_INST: return "GOTO_INST";
    case HANDLER_INST: return "HANDLER_INST";
    case IF_INST: return "IF_INST";
    case LABELS_INST: return "LABELS_INST";
    case QUOTE_INST: return "QUOTE_INST";
    case RETURN_INST: return "RETURN_INST";
    case SWITCH_ENV_INST: return "SWITCH_ENV_INST";
    case THROW_INST: return "THROW_INST";
    case TRACE_INST: return "TRACE_INST";
    case UNWIND_PROTECT_INST: return "UNWIND_PROTECT_INST";
    default: xassert(FALSE); return NULL;
  }
}

#define sint_p(o) ((((intptr_t)o) & 1) == 1)
#define sint_val(o) ((int)(((intptr_t)o) >> 1))
#define sint(i) ((object)((((uintptr_t)i) << 1) | 1))
#define top_inst() (sint_val(fs[ip]))
#define get_frame_var(base_ip, n) (fs[base_ip + 2 + n])
#define set_frame_var(base_ip, n, o) (fs[base_ip + 2 + n] = o)
#define frame_size(inst) (inst & INST_SIZE_MASK)
#define next_ip(base_ip) (base_ip + frame_size(sint_val(fs[base_ip])))
#define prev_ip(base_ip) (sint_val(fs[base_ip + 1]))
#define gen0(inst) gen(inst);
#define gen1(inst, lv0) \
{ \
  gen0(inst); \
  set_frame_var(ip, 0, lv0); \
}
#define gen2(inst, lv0, lv1) \
{ \
  gen0(inst); \
  set_frame_var(ip, 0, lv0); \
  set_frame_var(ip, 1, lv1); \
}
#define fb_gen0(inst) xarray_add(&fb, sint(inst));
#define fb_gen1(inst, o) \
{ \
  xarray_add(&fb, o); \
  fb_gen0(inst); \
}

STATIC void gen(int inst)
{
  if (sp > FRAME_STACK_SIZE - STACK_GAP) ip_mark_error("stack over flow");
  fs[sp + 1] = sint(ip);
  fs[sp] = sint(inst);
  ip = sp;
  sp = next_ip(ip);
}

STATIC void pop_frame(void)
{
  sp = ip;
  ip = prev_ip(ip);
}

STATIC void fb_flush(void)
{
  int i;
  object o;
  i = fb.size - 1;
  while (i > 0) {
    o = fb.elt[i];
    gen(sint_val(o));
    i--;
    switch (frame_size(sint_val(o))) {
      case 2: break;
      case 3: o = fb.elt[i]; set_frame_var(ip, 0, o); i--; break;
      default: xassert(FALSE); break;
    }
  }
  xarray_reset(&fb);
}

STATIC void pop_swith_env_frame(void)
{
  reg[1] = get_frame_var(ip, 0);
  pop_frame();
}

STATIC void pop_rewinding(void)
{
  switch (top_inst()) {
    case SWITCH_ENV_INST: pop_swith_env_frame(); break;
    case UNWIND_PROTECT_INST: xassert(FALSE); break;    // must be protected.
    default: pop_frame(); break;
  }
}

STATIC void gen_apply_frame(object operator)
{
  gen0(FENCE_INST);
  gen1(APPLY_INST, operator);
}

STATIC void gen_eval_frame(object o)
{
  if ((reg[0] = o)->header & EVAL_INST_MASK) gen0(EVAL_INST);
}

STATIC void gen_eval_args_frame(object args)
{
  if (args == object_nil) reg[0] = object_nil;
  else {
    gen2(EVAL_ARGS_INST, args->cons.cdr, object_nil);
    gen_eval_frame(args->cons.car);
  }
}

STATIC void gen_eval_sequential_frame(object args)
{
  if (args == object_nil) reg[0] = object_nil;
  else gen1(EVAL_SEQUENTIAL_INST, args);
}

STATIC void gen_if_frame(object args)
{
  if (args == object_nil) return;
  if (args->cons.cdr != object_nil) gen1(IF_INST, args->cons.cdr);
  gen_eval_frame(args->cons.car);
}

STATIC void gen_switch_env_frame(object env)
{
  gen1(SWITCH_ENV_INST, reg[1]);
  reg[1] = gc_new_env(env);
}

STATIC int same_symbol_keyword_p(object sym, object key)
{
  xassert(typep(sym, SYMBOL) && typep(key, KEYWORD));
  if (sym->barray.size != key->barray.size) return FALSE;
  return memcmp(sym->barray.elt, key->barray.elt, sym->barray.size) == 0;
}

STATIC int valid_keyword_p(object params, object args)
{
  object p, s;
  while (args != object_nil) {
    if (!typep(args->cons.car, KEYWORD)) {
      ip_mark_exception("expected keyword parameter");
      return FALSE;
    }
    p = params;
    while (p != object_nil) {
      s = p->cons.car;
      if (typep(s, CONS)) s = s->cons.car;
      xassert(typep(s, SYMBOL));
      if (same_symbol_keyword_p(s, args->cons.car)) break;
      p = p->cons.cdr;
    }
    if (p == object_nil) {
      ip_mark_exception("undeclared keyword parameter");
      return FALSE;
    }
    if ((args = args->cons.cdr) == object_nil) {
      ip_mark_exception("expected keyword parameter value");
      return FALSE;
    }
    args = args->cons.cdr;
  }
  return TRUE;
}

STATIC void parse_lambda_list(object env, object params, object args)
{
  object o, pre, k, v, def_v, sup_k;
  // parse required parameter
  while (params != object_nil && !typep(params->cons.car, KEYWORD)) {
    if (args == object_nil) {
      ip_mark_too_few_arguments_error();
      return;
    }
    if (!typep(params->cons.car, CONS)) {
      fb_gen1(QUOTE_INST, args->cons.car);
      fb_gen1(BIND_INST, params->cons.car);
    } else if (!listp(args->cons.car)) {
      mark_illegal_args_exception();
      return;
    } else {
      parse_lambda_list(env, params->cons.car, args->cons.car);
      if (ip_trap_code != TRAP_NONE) return;
    }
    params = params->cons.cdr;
    args = args->cons.cdr;
  }
  // parse optional parameter
  if (params->cons.car == object_opt) {
    params = params->cons.cdr;
    while (params != object_nil && !typep(params->cons.car, KEYWORD)) {
      o = params->cons.car;
      def_v = sup_k = NULL;
      if (!typep(o, CONS)) k = o;
      else {
        k = o->cons.car;
        def_v = (o = o->cons.cdr)->cons.car;
        if ((o = o->cons.cdr) != object_nil) sup_k = o->cons.car;
      }
      params = params->cons.cdr;
      if (sup_k != NULL) {
        v = object_bool(args != object_nil);
        fb_gen1(QUOTE_INST, v);
        fb_gen1(BIND_INST, sup_k);
      }
      if (args != object_nil) {
        fb_gen1(QUOTE_INST, args->cons.car);
        fb_gen1(BIND_INST, k);
        args = args->cons.cdr;
      } else {
        if (def_v == NULL) {
          fb_gen1(QUOTE_INST, object_nil);
          fb_gen1(BIND_INST, k);
        } else {
          fb_gen1(QUOTE_INST, def_v);
          fb_gen0(EVAL_INST);
          fb_gen1(BIND_INST, k);
        }
      }
    }
  }
  // parse rest parameter
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    fb_gen1(QUOTE_INST, args);
    fb_gen1(BIND_INST, params->cons.car);
    return;
  }
  // parse keyword parameter
  if (params->cons.car == object_key) {
    params = params->cons.cdr;
    if (!valid_keyword_p(params, args)) return;
    while (params != object_nil) {
      o = params->cons.car;
      v = def_v = sup_k = NULL;
      if (!typep(o, CONS)) k = o;
      else {
        k = o->cons.car;
        def_v = (o = o->cons.cdr)->cons.car;
        if ((o = o->cons.cdr) != object_nil) sup_k = o->cons.car;
      }
      o = args;
      pre = NULL;
      while (o != object_nil) {
        if (same_symbol_keyword_p(k,o->cons.car)) {
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
        fb_gen1(QUOTE_INST, object_bool(v != NULL));
        fb_gen1(BIND_INST, sup_k);
      }
      if (v != NULL) {
        fb_gen1(QUOTE_INST, v);
        fb_gen1(BIND_INST, k);
      }
      else {
        if (def_v == NULL) {
          fb_gen1(QUOTE_INST, object_nil);
          fb_gen1(BIND_INST, k);
        } else {
          fb_gen1(QUOTE_INST, def_v);
          fb_gen0(EVAL_INST);
          fb_gen1(BIND_INST, k);
        }
      }
      params = params->cons.cdr;
    }
  }
  if (args != object_nil) ip_mark_too_many_arguments_error();
}

STATIC void pop_apply_frame(void)
{
  object operator;
  operator = get_frame_var(ip, 0);
  pop_frame();
  gen_switch_env_frame(operator->lambda.env);
  parse_lambda_list(reg[1], operator->lambda.params, reg[0]);
  gen_eval_sequential_frame(operator->lambda.body);
  fb_flush();
}

STATIC void pop_apply_prim_frame(void)
{
  object args;
  int (*prim)(int, object, object *);
  args = reg[0];
  prim = get_frame_var(ip, 0)->prim;
  pop_frame();
  if ((*prim)(object_list_len(args), args, &(reg[0]))) return;
  if (error_msg == NULL) ip_mark_exception("primitive failed");
}

STATIC void pop_assert_frame(void)
{
  pop_frame();
  if (reg[0] == object_nil) ip_mark_error("assert failed");
}

STATIC void pop_bind_frame(void)
{
  symbol_bind(reg[1], get_frame_var(ip, 0), reg[0]);
  pop_frame();
}

STATIC void pop_bind_propagation_frame(void)
{
  symbol_bind_propagation(reg[1], get_frame_var(ip, 0), reg[0]);
  pop_frame();
}

STATIC void pop_eval_frame(void)
{
  object s;
  pop_frame();
  switch (type(reg[0])) {
    case SYMBOL:
      if ((s = symbol_find_propagation(reg[1], reg[0])) == NULL) {
        gen1(TRACE_INST, reg[0]);
        ip_mark_exception("unbind symbol");
        return;
      }
      reg[0] = s;
      return;
    case CONS:
      gen1(TRACE_INST, reg[0]);
      gen1(FETCH_OPERATOR_INST, reg[0]->cons.cdr);
      gen_eval_frame(reg[0]->cons.car);
      return;
    case MACRO:
    case LAMBDA:
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

STATIC void pop_unwind_protect_frame(void)
{
  object body;
  body = get_frame_var(ip, 0);
  pop_frame();
  gen_eval_sequential_frame(body);
}

STATIC void pop_goto_frame(void)
{
  object o, label;
  label = reg[0];
  if (!typep(label, KEYWORD)) {
    ip_mark_exception("label must be keyword");
    return;
  }
  while (TRUE) {
    if (ip < 0) {
      ip_mark_exception("labels context not found");
      return;
    }
    if (top_inst() == UNWIND_PROTECT_INST) {
      o = get_frame_var(ip, 0);
      pop_frame();
      gen0(GOTO_INST);
      gen1(QUOTE_INST, label);
      gen_eval_sequential_frame(o);
      return;
    }
    if (top_inst() == LABELS_INST) break;
    pop_rewinding();
  }
  o = get_frame_var(ip, 0);
  while (TRUE) {
    if (o == object_nil) {
      ip_mark_exception("label not found");
      return;
    }
    if (o->cons.car == label) break;
    o = o->cons.cdr;
  }
  gen_eval_sequential_frame(o);
}

STATIC void pop_fetch_handler_frame(void)
{
  object handler, body;
  handler = reg[0];
  body = get_frame_var(ip, 0);
  pop_frame();
  if (!typep(handler, LAMBDA)) {
    ip_mark_exception("require exception handler");
    return;
  }
  if (object_list_len(handler->lambda.params) != 1) {
    ip_mark_exception("handler parameter must be one required parameter");
    return;
  }
  gen1(HANDLER_INST, handler);
  gen_eval_sequential_frame(body);
}

STATIC void pop_fetch_operator_frame(void)
{
  object f, args;
  int (*special)(int, object);
  args = get_frame_var(ip, 0);
  pop_frame();
  switch (type(reg[0])) {
    case SYMBOL:
      if ((f = splay_find(object_special_splay, reg[0])) != NULL) {
        special = f->special;
        (*special)(object_list_len(args), args);
        return;
      }
      if ((f = splay_find(object_prim_splay, reg[0])) != NULL) {
        gen1(APPLY_PRIM_INST, f);
        gen_eval_args_frame(args);
        return;
      }
      break;
    case MACRO:
      gen0(EVAL_INST);
      gen_apply_frame(reg[0]);
      reg[0] = args;
      return;
    case LAMBDA:
      gen_apply_frame(reg[0]);
      gen_eval_args_frame(args);
      return;
    default: break;
  }
  ip_mark_exception("is not a operator");
}

STATIC void pop_eval_args_frame(void)
{
  object rest, acc;
  rest = get_frame_var(ip, 0);
  acc = gc_new_cons(reg[0], get_frame_var(ip, 1));
  set_frame_var(ip, 1, acc);
  if (rest == object_nil) {
    pop_frame();
    reg[0] = object_reverse(acc);
  } else {
    set_frame_var(ip, 0, rest->cons.cdr);
    gen_eval_frame(rest->cons.car);
  }
}

STATIC void pop_eval_sequential_frame(void)
{
  object args;
  args = get_frame_var(ip, 0);
  if (args == object_nil) pop_frame();
  else {
    set_frame_var(ip, 0, args->cons.cdr);
    gen_eval_frame(args->cons.car);
  }
}

static void pop_if_frame(void)
{
  object args;
  args = get_frame_var(ip, 0);
  pop_frame();
  if (reg[0] != object_nil) gen_eval_frame(args->cons.car);
  else if ((args = args->cons.cdr) != object_nil) gen_if_frame(args);
}

STATIC void pop_return_frame(void)
{
  object args;
  while (sp > 0) {
    switch (top_inst()) {
      case FENCE_INST:
        return;
      case UNWIND_PROTECT_INST: 
        args = get_frame_var(ip, 0);
        pop_frame();
        gen0(RETURN_INST);
        gen1(QUOTE_INST, reg[0]);
        gen_eval_sequential_frame(args);
        return;
      default:
        pop_rewinding();
        break;
    }
  }
}

STATIC object call_stack(int s, int e)
{
  int i;
  object o;
  o = object_nil;
  for (i = s; i < e; i = next_ip(i)) {
    if (sint_val(fs[i]) == TRACE_INST)
      o = gc_new_cons(get_frame_var(i, 0), o);
  }
  return o;
}

STATIC void exit1(void)
{
  char buf[MAX_STR_LEN];
  object o;
  o = call_stack(0, ip);
  printf("Error -- %s\n", object_describe(reg[0], buf));
  while (o != object_nil) {
    printf("	at: %s\n", object_describe(o->cons.car, buf));
    o = o->cons.cdr;
  }
  exit(1);
}

STATIC void pop_throw_frame(void)
{
  object body, handler;
  int s, e;
  e = ip;
  if (reg[3] != object_nil) e = reg[3]->xint.val;
  pop_frame();
  while (TRUE) {
    if (ip < 0) {
      ip = e;
      exit1();
    }
    if (top_inst() == UNWIND_PROTECT_INST) {
      body = get_frame_var(ip, 0);
      pop_frame();
      reg[3] = gc_new_xint(e);
      gen0(THROW_INST);
      gen1(QUOTE_INST, reg[0]);
      gen_eval_sequential_frame(body);
      return;
    }
    if (top_inst() == HANDLER_INST) {
      s = ip;
      ip = e;
      reg[2] = call_stack(s, e);
      ip = s;
      reg[3] = object_nil;
      handler = get_frame_var(ip, 0);
      pop_frame();
      break;
    }
    pop_rewinding();
  }
  gen_apply_frame(handler);
  reg[0] = gc_new_cons(reg[0], object_nil);
}

// special forms

/*
 * <lambda_list> ::= [<required_params>] 
 *                   [:opt <xparams>]
 *                   { [:rest <param>] | [:key <xparams>] }
 * <required_params> ::= <param> <param> ...
 * <xparams> ::= <xparam> <xparam> ...
 * <xparam> ::= { <param> | (<param> <initial_value> [<supplyp>]) }
 */
STATIC int valid_xparam_p(object o)
{
  o = o->cons.car;
  if (typep(o, SYMBOL)) return TRUE;
  return typep(o, CONS) && typep(o->cons.car, SYMBOL)
    && typep(o = o->cons.cdr, CONS)
    && (o->cons.cdr == object_nil
        || (typep(o = o->cons.cdr, CONS)
          && typep(o->cons.car, SYMBOL) && o->cons.cdr == object_nil));
}

STATIC int parse_params(object *o)
{
  *o = (*o)->cons.cdr;
  if (!valid_xparam_p(*o)) return FALSE;
  *o = (*o)->cons.cdr;
  while (TRUE) {
    if (*o == object_nil) break;
    if (typep((*o)->cons.car, KEYWORD)) break;
    if (!valid_xparam_p(*o)) return FALSE;
    *o = (*o)->cons.cdr;
  }
  return TRUE;
}

STATIC int valid_lambda_list_p(object params, int nest_p)
{
  int type;
  while (TRUE) {
    if (!listp(params)) return FALSE;
    if (params == object_nil) return TRUE;
    type = type(params->cons.car);
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
    if (!typep(params->cons.car, SYMBOL)) return FALSE;
    params = params->cons.cdr;
  } else if (params->cons.car == object_key) {
    if (!parse_params(&params)) return FALSE;
  }
  return params == object_nil;
}

SPECIAL(let)
{
  object args, s;
  if (!ip_ensure_arguments(argc, 1, FALSE)) return FALSE;
  if (!listp((args = argv->cons.car))) {
    ip_mark_exception("argument must be list");
    return FALSE;
  }
  if (args != object_nil) gen_switch_env_frame(reg[1]);
  gen_eval_sequential_frame(argv->cons.cdr);
  while (args != object_nil) {
    if (!typep((s = args->cons.car), SYMBOL)) {
      ip_mark_exception("argument must be symbol");
      return FALSE;
    }
    if ((args = args->cons.cdr) == object_nil) {
      ip_mark_exception("argument must be association list");
      return FALSE;
    }
    fb_gen1(QUOTE_INST, args->cons.car);
    fb_gen0(EVAL_INST);
    fb_gen1(BIND_INST, s);
    args = args->cons.cdr;
  }
  fb_flush();
  return TRUE;
}

SPECIAL(dynamic)
{
  int i;
  object e, s, v;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!typep((s = argv->cons.car), SYMBOL)) {
    ip_mark_exception("argument must be symbol");
    return FALSE;
  }
  i = ip;
  e = reg[1];
  while (TRUE) {
    if ((v = symbol_find(e, s)) != NULL) break;
    while ((i = prev_ip(i)) > 0) {
      if (sint_val(fs[i]) == SWITCH_ENV_INST) {
        e = get_frame_var(i, 0);
        break;
      }
    }
    if (i < 0) {
      ip_mark_exception("unbind symbol");
      return FALSE;
    }
  }
  reg[0] = v;
  return TRUE;
}

SPECIAL(symbol_bind)
{
  object s;
  if (argc == 0) return TRUE;
  if (argc % 2 != 0) {
    ip_mark_exception("must be pair");
    return FALSE;
  }
  while (argc != 0) {
    s = argv->cons.car;
    if (!typep(s, SYMBOL)) {
      ip_mark_exception("cannot bind except symbol");
      return FALSE;
    }
    if (s == object_nil) {
      ip_mark_exception("cannot bind nil");
      return FALSE;
    }
    argv = argv->cons.cdr;
    fb_gen1(QUOTE_INST, argv->cons.car);
    fb_gen0(EVAL_INST);
    fb_gen1(BIND_PROPAGATION_INST, s);
    argv = argv->cons.cdr;
    argc -= 2;
  }
  fb_flush();
  return TRUE;
}

SPECIAL(begin)
{
  gen_eval_sequential_frame(argv);
  return TRUE;
}

SPECIAL(macro)
{
  object params;
  if (!ip_ensure_arguments(argc, 3, FALSE)) return FALSE;
  if (!typep(argv->cons.car, SYMBOL)) {
    ip_mark_exception("required macro name");
    return FALSE;
  }
  gen1(BIND_PROPAGATION_INST, argv->cons.car);
  argv = argv->cons.cdr;
  if (!valid_lambda_list_p(params = argv->cons.car, TRUE)) {
    mark_illegal_args_exception();
    return FALSE;
  }
  reg[0] = gc_new_macro(reg[1], params, argv->cons.cdr);
  return TRUE;
}

SPECIAL(lambda)
{
  object params;
  if (!ip_ensure_arguments(argc, 2, FALSE)) return FALSE;
  params = argv->cons.car;
  if (!valid_lambda_list_p(params, FALSE)) {
    mark_illegal_args_exception();
    return FALSE;
  }
  reg[0] = gc_new_lambda(reg[1], params, argv->cons.cdr);
  return TRUE;
}

SPECIAL(quote)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  reg[0] = argv->cons.car;
  return TRUE;
}

SPECIAL(if)
{
  if (!ip_ensure_arguments(argc, 2, FALSE)) return FALSE;
  gen_if_frame(argv);
  return TRUE;
}

SPECIAL(unwind_protect)
{
  if (!ip_ensure_arguments(argc, 2, FALSE)) return FALSE;
  gen1(UNWIND_PROTECT_INST, argv->cons.cdr);
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

SPECIAL(labels)
{
  gen1(LABELS_INST, argv);
  gen_eval_sequential_frame(argv);
  return TRUE;
}

SPECIAL(goto)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  gen0(GOTO_INST);
  reg[0] = argv->cons.car;
  return TRUE;
}

SPECIAL(basic_throw)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  gen0(THROW_INST);
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

SPECIAL(basic_catch)
{
  if (!ip_ensure_arguments(argc, 1, FALSE)) return FALSE;
  gen1(FETCH_HANDLER_INST, argv->cons.cdr);
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

SPECIAL(return)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  gen0(RETURN_INST);
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

SPECIAL(assert)
{
#ifndef NDEBUG
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  gen0(ASSERT_INST);
  gen_eval_frame(argv->cons.car);
#endif
  return TRUE;
}

PRIM(eval)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  gen_eval_frame(argv->cons.car);
  return TRUE;
}

PRIM(apply)
{
  object f;
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  switch (type(argv->cons.car)) {
    case SYMBOL:
      if ((f = splay_find(object_prim_splay, argv->cons.car)) == NULL) break;
      else {
        gen1(APPLY_PRIM_INST, f);
        reg[0] = argv->cons.cdr->cons.car;
        return TRUE;
      }
    case LAMBDA:
      gen_apply_frame(argv->cons.car);
      reg[0] = argv->cons.cdr->cons.car;
      return TRUE;
    default: break;
  }
  ip_mark_exception("requires function or symbol(built in function) to apply");
  return FALSE;
}

PRIM(expand_macro)
{
  object f, args;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  reg[0] = argv->cons.car;
  if (!typep(reg[0], CONS)) return TRUE;
  f = reg[0]->cons.car;
  args = reg[0]->cons.cdr;
  if (!typep(f, SYMBOL)) return TRUE;
  if ((f = symbol_find_propagation(reg[1], f)) == NULL) return TRUE;
  if (!typep(f, MACRO)) return TRUE;
  gen_apply_frame(f);
  reg[0] = args;
  return TRUE;
}

PRIM(bound_p)
{
  object s;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!typep((s = argv->cons.car), SYMBOL)) {
    ip_mark_exception("required symbol");
    return FALSE;
  }
  *result = object_bool(symbol_find_propagation(reg[1], s) != NULL);
  return TRUE;
}

PRIM(call_stack)
{
  if (!ip_ensure_no_args(argc)) return FALSE;
  *result = reg[2];
  return TRUE;
}

STATIC void trap(void)
{
  object e;
  switch (ip_trap_code) {
    case TRAP_EXCEPTION:
    case TRAP_ERROR:
      xassert(error_msg != NULL);
      if (ip_trap_code == TRAP_EXCEPTION) e = object_Exception;
      else e = object_Error;
      gen0(THROW_INST);
      reg[0] = gc_new_throwable(e, error_msg);
      ip_trap_code = TRAP_NONE;
      error_msg = NULL;
      break;
    default:
      ip_trap_code = TRAP_NONE;
      break;
  }
}

STATIC void ip_main(void)
{
  reg[0] = object_nil;
  reg[1] = object_toplevel;
  reg[2] = object_nil;
  reg[3] = object_nil;
  gen_apply_frame(object_boot);
  while (ip != -1) {
    xassert(ip >= 0);
    if (ip_trap_code != TRAP_NONE) trap();
    if(cycle % IP_POLLING_INTERVAL == 0) {
      if (reg[3] == object_nil) gc_chance();
    }
    switch (top_inst()) {
      case APPLY_INST: pop_apply_frame(); break;
      case APPLY_PRIM_INST: pop_apply_prim_frame(); break;
      case ASSERT_INST: pop_assert_frame(); break;
      case BIND_INST: pop_bind_frame(); break;
      case BIND_PROPAGATION_INST: pop_bind_propagation_frame(); break;
      case EVAL_INST: pop_eval_frame(); break;
      case EVAL_ARGS_INST: pop_eval_args_frame(); break;
      case EVAL_SEQUENTIAL_INST: pop_eval_sequential_frame(); break;
      case FENCE_INST: pop_frame(); break;
      case FETCH_HANDLER_INST: pop_fetch_handler_frame(); break;
      case FETCH_OPERATOR_INST: pop_fetch_operator_frame(); break;
      case GOTO_INST: pop_goto_frame(); break;
      case HANDLER_INST: pop_frame(); break;
      case IF_INST: pop_if_frame(); break;
      case LABELS_INST: pop_frame(); break;
      case QUOTE_INST: reg[0] = get_frame_var(ip, 0); pop_frame(); break;
      case RETURN_INST: pop_return_frame(); break;
      case SWITCH_ENV_INST: pop_swith_env_frame(); break;
      case THROW_INST: pop_throw_frame(); break;
      case TRACE_INST: pop_frame(); break;
      case UNWIND_PROTECT_INST: pop_unwind_protect_frame(); break;
      default: xassert(FALSE); break;
    }
    cycle++;
  }
}

void ip_mark(void)
{
  int i;
  for (i = 0; i < REG_SIZE; i++) gc_mark(reg[i]);
  for (i = 0; i < sp; i++) {
    if (!sint_p(fs[i])) gc_mark(fs[i]);
  }
  for (i = 0; byte_range_p(i); i++) gc_mark(object_bytes[i]);
}

void ip_start(void)
{
  sp = 0;
  ip = -1;
  cycle = 0;
  xarray_init(&fb);
  ip_trap_code = TRAP_NONE;
  ip_main();
}
