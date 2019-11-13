// interpreter

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "xbarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

/*
 * registers:
 *   0 -- inst argument.
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

static void ip_mark_too_few_arguments_error(void)
{
  ip_mark_error("too few arguments");
}

static void ip_mark_too_many_arguments_error(void)
{
  ip_mark_error("too many arguments");
}

int ip_ensure_no_args(int argc)
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

static void mark_illegal_parameter_error()
{
  ip_mark_error("illegal parameter list");
}

// symbol

static object symbol_find(object e, object s)
{
  xassert(typep(e, ENV));
  return xsplay_find(&e->env.binding, s);
}

static object symbol_find_propagation(object e, object s)
{
  object v;
  while (e != object_nil) {
    if ((v = symbol_find(e, s)) != NULL) break;
    e = e->env.top;
  }
  return v;
}

static void symbol_bind(object e, object s, object v)
{
  xassert(typep(e, ENV));
  xsplay_replace(&e->env.binding, s, v);
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

// inst

#define STACK_GAP 5
#define INST_STACK_SIZE 1000

static int sp;
static object fs[INST_STACK_SIZE];
static struct xarray fb;

static void fb_add(object f)
{
  xarray_add(&fb, f);
}

static void fb_reset(void)
{
  xarray_reset(&fb);
}

static void fs_push(object f);
static void fb_flush(void)
{
  int i;
  for (i = fb.size - 1; i >= 0; i--) fs_push(fb.elt[i]);
}

// instructions
#define APPLY_INST 0
#define APPLY_PRIM_INST 1
#define ASSERT_INST 2
#define BIND_INST 3
#define BIND_PROPAGATION_INST 4
#define EVAL_INST 5
#define EVAL_ARGS_INST 6
#define EVAL_SEQUENTIAL_INST 7
#define FENCE_INST 8
#define FETCH_HANDLER_INST 9
#define FETCH_OPERATOR_INST 10
#define GOTO_INST 11
#define HANDLER_INST 12
#define IF_INST 13
#define LABELS_INST 14
#define QUOTE_INST 15
#define RETURN_INST 16
#define SWITCH_ENV_INST 17
#define THROW_INST 18
#define TRACE_INST 19
#define UNWIND_PROTECT_INST 20
#define LAST_INST 21

int inst_size(object o)
{
  xassert(typep(o, XINT));
  switch (o->xint.val) {
    case ASSERT_INST:
    case EVAL_INST:
    case FENCE_INST:
    case GOTO_INST:
    case RETURN_INST:
    case THROW_INST:
      return 0;
    case APPLY_INST:
    case APPLY_PRIM_INST:
    case BIND_INST:
    case BIND_PROPAGATION_INST:
    case EVAL_SEQUENTIAL_INST:
    case FETCH_HANDLER_INST:
    case FETCH_OPERATOR_INST:
    case HANDLER_INST:
    case IF_INST:
    case LABELS_INST:
    case QUOTE_INST:
    case SWITCH_ENV_INST:
    case TRACE_INST:
    case UNWIND_PROTECT_INST:
      return 1;
    case EVAL_ARGS_INST:
      return 2;
    default:
      xassert(FALSE);
      return FALSE;
  }
}

char *inst_name(object o)
{
  xassert(typep(o, XINT));
  switch (o->xint.val) {
    case APPLY_INST: return "APPLY_INST";
    case APPLY_PRIM_INST: return "APPLY_PRIM_INST";
    case ASSERT_INST: return "ASSERT_INST";
    case BIND_INST: return "BIND_INST";
    case BIND_PROPAGATION_INST: return "BIND_PROPAGATION_INST";
    case EVAL_ARGS_INST: return "EVAL_ARGS_INST";
    case EVAL_INST: return "EVAL_INST";
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
    case TRACE_INST: return "TRACE_INST";
    case THROW_INST: return "THROW_INST";
    default: xassert(FALSE); return NULL;
  }
}

static object inst(int type)
{
  return object_bytes[type];
}

static object gen_inst0(int type)
{
  return gc_new_cons(inst(type), object_nil);
}

static object gen_inst1(int type, object o)
{
  return gc_new_cons(inst(type), gc_new_cons(o, object_nil));
}

static object gen_inst2(int type, object o, object p)
{
  return gc_new_cons(inst(type), gc_new_cons(o, gc_new_cons(p, object_nil)));
}

static object fs_top(void)
{
  xassert(sp > 0);
  return fs[sp - 1];
}

static object fs_nth(int n)
{
  xassert(0 <= n && n < sp);
  return fs[n];
}

static void fs_push(object f)
{
  if (sp > INST_STACK_SIZE - STACK_GAP) ip_mark_error("stack over flow");
  else if (sp + 3 > INST_STACK_SIZE) xerror("stack over flow.");
  fs[sp] = f;
  sp++;
}

static object fs_pop(void)
{
  object top;
  top = fs_top();
  --sp;
  return top;
}

static void pop_switch_env(void);
static void pop_unwind_protect_inst(void);

static void fs_rewind_pop(void)
{
  switch (fs_top()->cons.car->xint.val) {
    case SWITCH_ENV_INST: pop_switch_env(); break;
    case UNWIND_PROTECT_INST: xassert(FALSE); break;    // must be protected.
    default: fs_pop(); break;
  }
}

static void push_fence_inst(void);

static void push_apply_inst(object operator)
{
  push_fence_inst();
  fs_push(gen_inst1(APPLY_INST, operator));
}

static void push_apply_prim_inst(object prim)
{
  fs_push(gen_inst1(APPLY_PRIM_INST, prim));
}

#ifndef NDEBUG
static void push_assert_inst(void)
{
  fs_push(gen_inst0(ASSERT_INST));
}
#endif

static object make_eval_inst(void)
{
  return gen_inst0(EVAL_INST);
}

static void push_eval_inst(void)
{
  fs_push(make_eval_inst());
}

static object make_bind_inst(object sym)
{
  return gen_inst1(BIND_INST, sym);
}

static object make_bind_propagation_inst(object sym)
{
  return gen_inst1(BIND_PROPAGATION_INST, sym);
}

static void push_eval_args_inst(object args)
{
  if (args == object_nil) reg[0] = object_nil;
  else {
    fs_push(gen_inst2(EVAL_ARGS_INST, args->cons.cdr, object_nil));
    push_eval_inst();
    reg[0] = args->cons.car;
  }
}

static void push_eval_sequential_inst(object args)
{
  if (args == object_nil) reg[0] = object_nil;
  else fs_push(gen_inst1(EVAL_SEQUENTIAL_INST, args));
}

static void push_fence_inst(void)
{
  fs_push(gen_inst0(FENCE_INST));
}

static void push_fetch_handler_inst(object body)
{
  fs_push(gen_inst1(FETCH_HANDLER_INST, body));
}

static void push_fetch_operator_inst(object args)
{
  fs_push(gen_inst1(FETCH_OPERATOR_INST, args));
}

static void push_goto_inst(void)
{
  fs_push(gen_inst0(GOTO_INST));
}

static void push_handler_inst(object handler)
{
  fs_push(gen_inst1(HANDLER_INST, handler));
}

static void push_if_inst(object args)
{
  if (args == object_nil) return;
  if (args->cons.cdr != object_nil) {
    fs_push(gen_inst1(IF_INST, args->cons.cdr));
  }
  push_eval_inst();
  reg[0] = args->cons.car;
}

static void push_labels_inst(object args)
{
  fs_push(gen_inst1(LABELS_INST, args));
}

static object make_quote_inst(object arg)
{
  return gen_inst1(QUOTE_INST, arg);
}

static void push_quote_inst(object arg)
{
  fs_push(make_quote_inst(arg));
}

static void push_return_inst(void)
{
  fs_push(gen_inst0(RETURN_INST));
}

static void push_switch_env_inst(object env)
{
  fs_push(gen_inst1(SWITCH_ENV_INST, reg[1]));
  reg[1] = gc_new_env(env);
}

static void push_throw_inst(void)
{
  fs_push(gen_inst0(THROW_INST));
}

static void push_trace_inst(void)
{
  fs_push(gen_inst1(TRACE_INST, reg[0]));
}

static void push_unwind_protect_inst(object argv)
{
  fs_push(gen_inst1(UNWIND_PROTECT_INST, argv));
}

static void parse_lambda_list(object env, object params, object args);
static void pop_apply_inst(void)
{
  object operator;
  operator = fs_pop()->cons.cdr->cons.car;
  push_switch_env_inst(operator->lambda.env);
  fb_reset();
  parse_lambda_list(reg[1], operator->lambda.params, reg[0]);
  push_eval_sequential_inst(operator->lambda.body);
  fb_flush();
}

static void pop_apply_prim_inst(void)
{
  object args;
  int (*prim)(int, object, object *);
  args = reg[0];
  prim = xsplay_find(&prim_splay, fs_pop()->cons.cdr->cons.car);
  if ((*prim)(object_list_len(args), args, &(reg[0]))) return;
  if (error_msg == NULL) ip_mark_error("primitive failed");
}

#ifndef NDEBUG
static void pop_assert_inst(void)
{
  fs_pop();
  if (reg[0] == object_nil) ip_mark_error("assert failed");
}
#endif

static void pop_bind_inst(void)
{
  symbol_bind(reg[1], fs_pop()->cons.cdr->cons.car, reg[0]);
}

static void pop_bind_propagation_inst(void)
{
  symbol_bind_propagation(reg[1], fs_pop()->cons.cdr->cons.car, reg[0]);
}

static void pop_eval_inst(void)
{
  object s;
  fs_pop();
  push_trace_inst();
  switch (type(reg[0])) {
    case MACRO:
    case LAMBDA:
    case XINT:
    case XFLOAT:
    case KEYWORD:
    case STRING:
    case BARRAY:
    case ARRAY:
      return;
    case SYMBOL:
      if ((s = symbol_find_propagation(reg[1], reg[0])) == NULL) {
        ip_mark_error("unbind symbol");
        return;
      }
      reg[0] = s;
      return;
    case CONS:
      push_fetch_operator_inst(reg[0]->cons.cdr);
      push_eval_inst();
      reg[0] = reg[0]->cons.car;
      return;
    default: xassert(FALSE);
  }
}

static void pop_goto_inst(void)
{
  object o, label;
  label = reg[0];
  if (!typep(label, KEYWORD)) {
    ip_mark_error("label must be keyword");
    return;
  }
  while (TRUE) {
    if (sp == 0) {
      ip_mark_error("labels context not found");
      return;
    }
    if (fs_top()->cons.car->xint.val == UNWIND_PROTECT_INST) {
      o = fs_pop()->cons.cdr->cons.car;
      push_goto_inst();
      push_quote_inst(label);
      push_eval_sequential_inst(o);
      return;
    }
    if (fs_top()->cons.car->xint.val == LABELS_INST) break;
    fs_rewind_pop();
  }
  o = fs_top()->cons.cdr->cons.car;
  while (TRUE) {
    if (o == object_nil) {
      ip_mark_error("label not found");
      return;
    }
    if (o->cons.car == label) break;
    o = o->cons.cdr;
  }
  push_eval_sequential_inst(o);
}

static void pop_fetch_handler_inst(void)
{
  object handler, body;
  handler = reg[0];
  body = fs_pop()->cons.cdr->cons.car;
  if (!typep(handler, LAMBDA)) {
    ip_mark_error("require exception handler");
    return;
  }
  if (object_list_len(handler->lambda.params) != 1) {
    ip_mark_error("handler parameter must be one required parameter");
    return;
  }
  push_handler_inst(handler);
  push_eval_sequential_inst(body);
}

static void pop_fetch_operator_inst(void)
{
  object args;
  int (*special)(int, object);
  args = fs_pop()->cons.cdr->cons.car;
  switch (type(reg[0])) {
    case SYMBOL:
      if ((special = xsplay_find(&special_splay, reg[0])) != NULL) {
        (*special)(object_list_len(args), args);
        return;
      }
      if (xsplay_find(&prim_splay, reg[0]) != NULL) {
        push_apply_prim_inst(reg[0]);
        push_eval_args_inst(args);
        return;
      }
      break;
    case MACRO:
      push_eval_inst();
      push_apply_inst(reg[0]);
      reg[0] = args;
      return;
    case LAMBDA:
      push_apply_inst(reg[0]);
      push_eval_args_inst(args);
      return;
    default: break;
  }
  ip_mark_error("is not a operator");
}

static void pop_eval_args_inst(void)
{
  object top;
  object rest, acc;
  top = fs_top();
  rest = top->cons.cdr->cons.car;
  acc = top->cons.cdr->cons.cdr->cons.car;
  acc = gc_new_cons(reg[0], acc);
  top->cons.cdr->cons.cdr->cons.car = acc;
  if (rest == object_nil) {
    fs_pop();
    reg[0] = object_reverse(acc);
  } else {
    top->cons.cdr->cons.car = rest->cons.cdr;
    push_eval_inst();
    reg[0] = rest->cons.car;
  }
}

static void pop_eval_sequential_inst(void)
{
  object args;
  args = fs_top()->cons.cdr->cons.car;
  if (args == object_nil) fs_pop();
  else {
    fs_top()->cons.cdr->cons.car = args->cons.cdr;
    push_eval_inst();
    reg[0] = args->cons.car;
  }
}

static void pop_if_inst(void)
{
  object args;
  args = fs_pop()->cons.cdr->cons.car;
  if (reg[0] != object_nil) {
    push_eval_inst();
    reg[0] = args->cons.car;
  } else if ((args = args->cons.cdr) != object_nil) push_if_inst(args);
}

static void pop_quote_inst(void)
{
  reg[0] = fs_pop()->cons.cdr->cons.car;
}

static void pop_switch_env(void)
{
  reg[1] = fs_pop()->cons.cdr->cons.car;
}

static void pop_return_inst(void)
{
  object args;
  while (sp != 0) {
    switch (fs_top()->cons.car->xint.val) {
      case FENCE_INST:
        return;
      case UNWIND_PROTECT_INST: 
        args = fs_pop()->cons.cdr->cons.car;
        push_return_inst();
        push_quote_inst(reg[0]);
        push_eval_sequential_inst(args);
        return;
      default:
        fs_rewind_pop();
        break;
    }
  }
}

static void ip_finish(void);

static void pop_throw_inst(void)
{
  object body, handler;
  int xsp;
  xsp = sp;
  while (TRUE) {
    if (sp == 0) {
      sp = xsp;
      ip_finish();
    }
    if (fs_top()->cons.car->xint.val == UNWIND_PROTECT_INST) {
      body = fs_pop()->cons.cdr->cons.car;
      push_throw_inst();
      push_quote_inst(reg[0]);
      push_eval_sequential_inst(body);
      return;
    }
    if (fs_top()->cons.car->xint.val == HANDLER_INST) {
      handler = fs_pop()->cons.cdr->cons.car;
      break;
    }
    fs_rewind_pop();
  }
  push_apply_inst(handler);
  reg[0] = gc_new_cons(reg[0], object_nil);
}

static void pop_unwind_protect_inst(void)
{
  push_eval_sequential_inst(fs_pop()->cons.cdr->cons.car);
}

// trace and debug

static object call_stack(void)
{
  int i;
  object o;
  i = 0;
  o = object_nil;
  while (i < sp) {
    if (fs_nth(i)->cons.car->xint.val == TRACE_INST)
      o = gc_new_cons(fs_nth(i)->cons.cdr->cons.car, o);
    i++;
  }
  return o;
}

static void ip_finish(void)
{
  char buf[MAX_STR_LEN];
  object o;
  o = call_stack();
  printf("Error -- %s\n", object_describe(reg[0], buf));
  while (o != object_nil) {
    printf("	at: %s\n", object_describe(o->cons.car, buf));
    o = o->cons.cdr;
  }
  exit(1);
}

// special/prim

static int same_symbol_keyword_p(object sym, object key)
{
  xassert(typep(sym, SYMBOL) && typep(key, KEYWORD));
  if (sym->barray.size != key->barray.size) return FALSE;
  return memcmp(sym->barray.elt, key->barray.elt, sym->barray.size) == 0;
}

static int valid_keyword_p(object params, object args)
{
  object p, s;
  while (args != object_nil) {
    if (!typep(args->cons.car, KEYWORD)) {
      ip_mark_error("expected keyword parameter");
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

// TODO lambda-list内のシンボルが一意であることのvalidate-lambda-listに追加する。
static void parse_lambda_list(object env, object params, object args)
{
  object o, pre, k, v, def_v, sup_k;
  // parse required parameter
  while (params != object_nil && !typep(params->cons.car, KEYWORD)) {
    if (args == object_nil) {
      ip_mark_too_few_arguments_error();
      return;
    }
    if (!typep(params->cons.car, CONS)) {
      fb_add(make_quote_inst(args->cons.car));
      fb_add(make_bind_inst(params->cons.car));
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
        fb_add(make_quote_inst(v));
        fb_add(make_bind_inst(sup_k));
      }
      if (args != object_nil) {
        fb_add(make_quote_inst(args->cons.car));
        fb_add(make_bind_inst(k));
        args = args->cons.cdr;
      } else {
        if (def_v == NULL) {
          fb_add(make_quote_inst(object_nil));
          fb_add(make_bind_inst(k));
        } else {
          fb_add(make_quote_inst(def_v));
          fb_add(make_eval_inst());
          fb_add(make_bind_inst(k));
        }
      }
    }
  }
  // parse rest parameter
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    fb_add(make_quote_inst(args));
    fb_add(make_bind_inst(params->cons.car));
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
        fb_add(make_quote_inst(object_bool(v != NULL)));
        fb_add(make_bind_inst(sup_k));
      }
      if (v != NULL) {
        fb_add(make_quote_inst(v));
        fb_add(make_bind_inst(k));
      }
      else {
        if (def_v == NULL) {
          fb_add(make_quote_inst(object_nil));
          fb_add(make_bind_inst(k));
        } else {
          fb_add(make_quote_inst(def_v));
          fb_add(make_eval_inst());
          fb_add(make_bind_inst(k));
        }
      }
      params = params->cons.cdr;
    }
  }
  if (args != object_nil) ip_mark_too_many_arguments_error();
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
static int valid_xparam_p(object o)
{
  o = o->cons.car;
  if (typep(o, SYMBOL)) return TRUE;
  return typep(o, CONS) && typep(o->cons.car, SYMBOL)
    && typep(o = o->cons.cdr, CONS)
    && (o->cons.cdr == object_nil
        || (typep(o = o->cons.cdr, CONS)
          && typep(o->cons.car, SYMBOL) && o->cons.cdr == object_nil));
}

static int parse_params(object *o)
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

static int valid_lambda_list_p(int object_type, object params)
{
  int type;
  while (TRUE) {
    if (params == object_nil) return TRUE;
    type = type(params->cons.car);
    if (type == KEYWORD) break;
    else if (type == SYMBOL) params = params->cons.cdr;
    else if (type == CONS) {
      if (object_type != MACRO || !valid_lambda_list_p(MACRO, params->cons.car))
        return FALSE;
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
    ip_mark_error("argument must be list");
    return FALSE;
  }
  if (args != object_nil) push_switch_env_inst(reg[1]);
  push_eval_sequential_inst(argv->cons.cdr);
  fb_reset();
  while (args != object_nil) {
    if (!typep((s = args->cons.car), SYMBOL)) {
      ip_mark_error("argument must be symbol");
      return FALSE;
    }
    if ((args = args->cons.cdr) == object_nil) {
      ip_mark_error("argument must be association list");
      return FALSE;
    }
    fb_add(make_quote_inst(args->cons.car));
    fb_add(make_eval_inst());
    fb_add(make_bind_inst(s));
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
    ip_mark_error("argument must be symbol");
    return FALSE;
  }
  i = sp - 1;
  e = reg[1];
  while (TRUE) {
    if ((v = symbol_find(e, s)) != NULL) break;
    while (--i >= 0) {
      if (fs[i]->cons.car->xint.val == SWITCH_ENV_INST) {
        e = fs[i]->cons.cdr->cons.car;
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

SPECIAL(symbol_bind)
{
  object s;
  if (argc == 0) return TRUE;
  if (argc % 2 != 0) {
    ip_mark_error("must be pair");
    return FALSE;
  }
  fb_reset();
  while (argc != 0) {
    s = argv->cons.car;
    if (!typep(s, SYMBOL)) {
      ip_mark_error("cannot bind except symbol");
      return FALSE;
    }
    if (s == object_nil) {
      ip_mark_error("cannot bind nil");
      return FALSE;
    }
    argv = argv->cons.cdr;
    fb_add(make_quote_inst(argv->cons.car));
    fb_add(make_eval_inst());
    fb_add(make_bind_propagation_inst(s));
    argv = argv->cons.cdr;
    argc -= 2;
  }
  fb_flush();
  return TRUE;
}

SPECIAL(begin)
{
  push_eval_sequential_inst(argv);
  return TRUE;
}

SPECIAL(macro)
{
  object params;
  if (!ip_ensure_arguments(argc, 3, FALSE)) return FALSE;
  if (!typep(argv->cons.car, SYMBOL)) {
    ip_mark_error("required macro name");
    return FALSE;
  } else {
    fs_push(make_bind_propagation_inst(argv->cons.car));
    argv = argv->cons.cdr;
  }
  if (!valid_lambda_list_p(MACRO, params = argv->cons.car)) {
    mark_illegal_parameter_error();
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
  if (!valid_lambda_list_p(LAMBDA, params)) {
    mark_illegal_parameter_error();
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
  push_if_inst(argv);
  return TRUE;
}

SPECIAL(unwind_protect)
{
  if (!ip_ensure_arguments(argc, 2, FALSE)) return FALSE;
  push_unwind_protect_inst(argv->cons.cdr);
  push_eval_inst();
  reg[0] = argv->cons.car;
  return TRUE;
}

SPECIAL(labels)
{
  push_labels_inst(argv);
  push_eval_sequential_inst(argv);
  return TRUE;
}

SPECIAL(goto)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  push_goto_inst();
  push_eval_inst();
  reg[0] = argv->cons.car;
  return TRUE;
}

SPECIAL(throw)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  push_throw_inst();
  push_eval_inst();
  reg[0] = argv->cons.car;
  return TRUE;
}

SPECIAL(basic_catch)
{
  if (!ip_ensure_arguments(argc, 1, FALSE)) return FALSE;
  push_fetch_handler_inst(argv->cons.cdr);
  push_eval_inst();
  reg[0] = argv->cons.car;
  return TRUE;
}

SPECIAL(return)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  push_return_inst();
  push_eval_inst();
  reg[0] = argv->cons.car;
  return TRUE;
}

SPECIAL(assert)
{
#ifndef NDEBUG
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  push_assert_inst();
  push_eval_inst();
  reg[0] = argv->cons.car;
#endif
  return TRUE;
}

PRIM(eval)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  push_eval_inst();
  reg[0] = argv->cons.car;
  return TRUE;
}

PRIM(apply)
{
  if (!ip_ensure_arguments(argc, 2, 2)) return FALSE;
  switch (type(argv->cons.car)) {
    case SYMBOL:
      if (xsplay_find(&prim_splay, argv->cons.car) == NULL) break;
      else {
        push_apply_prim_inst(argv->cons.car);
        reg[0] = argv->cons.cdr->cons.car;
        return TRUE;
      }
    case LAMBDA:
      push_apply_inst(argv->cons.car);
      reg[0] = argv->cons.cdr->cons.car;
      return TRUE;
    default: break;
  }
  ip_mark_error("requires function or symbol(built in function) to apply");
  return FALSE;
}

PRIM(bound_p)
{
  object s;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  if (!typep((s = argv->cons.car), SYMBOL)) {
    ip_mark_error("required symbol");
    return FALSE;
  }
  *result = object_bool(symbol_find_propagation(reg[1], s) != NULL);
  return TRUE;
}

PRIM(call_stack)
{
  if (!ip_ensure_no_args(argc)) return FALSE;
  *result = call_stack()->cons.cdr;    // remove own.
  return TRUE;
}

static void trap(void)
{
  switch (ip_trap_code) {
    case TRAP_ERROR:
      push_throw_inst();
      xassert(error_msg != NULL);
      reg[0] = gc_new_barray_from(STRING, strlen(error_msg), error_msg);
      ip_trap_code = TRAP_NONE;
      error_msg = NULL;
      break;
    default:
      ip_trap_code = TRAP_NONE;
      break;
  }
}

static object ip_main(void)
{
  reg[0] = object_nil;
  reg[1] = object_toplevel;
  push_apply_inst(object_boot);
  while (TRUE) {
    xassert(sp >= 0);
    if (ip_trap_code != TRAP_NONE) trap();
    if (sp == 0) break;
    switch (fs_top()->cons.car->xint.val) {
      case APPLY_INST: pop_apply_inst(); break;
      case APPLY_PRIM_INST: pop_apply_prim_inst(); break;
#ifndef NDEBUG
      case ASSERT_INST: pop_assert_inst(); break;
#endif
      case BIND_INST: pop_bind_inst(); break;
      case BIND_PROPAGATION_INST: pop_bind_propagation_inst(); break;
      case EVAL_INST: pop_eval_inst(); break;
      case EVAL_ARGS_INST: pop_eval_args_inst(); break;
      case EVAL_SEQUENTIAL_INST: pop_eval_sequential_inst(); break;
      case FENCE_INST: fs_pop(); break;
      case FETCH_HANDLER_INST: pop_fetch_handler_inst(); break;
      case FETCH_OPERATOR_INST: pop_fetch_operator_inst(); break;
      case GOTO_INST: pop_goto_inst(); break;
      case HANDLER_INST: fs_pop(); break;
      case IF_INST: pop_if_inst(); break;
      case LABELS_INST: fs_pop(); break;
      case QUOTE_INST: pop_quote_inst(); break;
      case RETURN_INST: pop_return_inst(); break;
      case SWITCH_ENV_INST: pop_switch_env(); break;
      case THROW_INST: pop_throw_inst(); break;
      case TRACE_INST: fs_pop(); break;
      case UNWIND_PROTECT_INST: pop_unwind_protect_inst(); break;
      default: xassert(FALSE);
    }
    cycle++;
  }
  return reg[0];
}

void ip_mark(void)
{
  // TODO mark fs, regs, ....
}

void ip_start(void)
{
  cycle = sp = 0;
  xarray_init(&fb);
  ip_trap_code = TRAP_NONE;
  ip_main();
}
