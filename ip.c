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
 *   0 -- frame argument.
 *   1 -- current environment.
 *   2 -- thrown object.
 *   3 -- stack frame pointer.
 */
#define REG_SIZE 4
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

static void st_push(object o)
{
  object p;
  p = symbol_find(object_toplevel, object_st);
  xassert(p != NULL);
  symbol_bind(object_toplevel, object_st, gc_new_cons(o, p));
}

static void st_pop(void)
{
  object o;
  o = symbol_find(object_toplevel, object_st);
  xassert(o != object_nil);
  symbol_bind(object_toplevel, object_st, o->cons.cdr);
}

// frame

#define STACK_GAP 5
#define FRAME_STACK_SIZE 1000

static int sp;
static struct frame *fs[FRAME_STACK_SIZE];
static struct xarray fb;

static void fb_add(struct frame *f)
{
  xarray_add(&fb, f);
}

static void fb_reset(void)
{
  xarray_reset(&fb);
}

static void fs_push(struct frame *f);
static void fb_flush(void)
{
  int i;
  for (i = fb.size - 1; i >= 0; i--) fs_push(fb.elt[i]);
}

struct frame {
  int type;
#define APPLY_FRAME 0
#define APPLY_PRIM_FRAME 1
#define ASSERT_FRAME 2
#define BIND_FRAME 3
#define BIND_LOCAL_VAR_FRAME 4
#define BIND_PROPAGATION_FRAME 5
#define EVAL_FRAME 6
#define EVAL_LOCAL_VAR_FRAME 7
#define EVAL_ARGS_FRAME 8
#define EVAL_SEQUENTIAL_FRAME 9
#define FETCH_HANDLER_FRAME 10
#define FETCH_OPERATOR_FRAME 11
#define HANDLER_FRAME 12
#define IF_FRAME 13
#define LABELS_FRAME 14
#define RETURN_ADDR_FRAME 15
#define RETURN_FRAME 16
#define SWITCH_ENV_FRAME 17
#define THROW_FRAME 18
  object local_vars[0];
};

static int frame_size(int type)
{
  switch (type) {
    case ASSERT_FRAME:
    case EVAL_FRAME:
    case RETURN_ADDR_FRAME:
    case RETURN_FRAME:
    case THROW_FRAME:
      return 0;
    case APPLY_FRAME:
    case APPLY_PRIM_FRAME:
    case BIND_FRAME:
    case BIND_PROPAGATION_FRAME:
    case EVAL_LOCAL_VAR_FRAME:
    case EVAL_SEQUENTIAL_FRAME:
    case FETCH_HANDLER_FRAME:
    case FETCH_OPERATOR_FRAME:
    case HANDLER_FRAME:
    case IF_FRAME:
    case LABELS_FRAME:
    case SWITCH_ENV_FRAME:
      return 1;
    case BIND_LOCAL_VAR_FRAME:
    case EVAL_ARGS_FRAME:
      return 2;
    default:
      xassert(FALSE);
      return FALSE;
  }
}

char *frame_name(int type)
{
  switch (type) {
    case APPLY_FRAME: return "APPLY_FRAME";
    case APPLY_PRIM_FRAME: return "APPLY_PRIM_FRAME";
    case ASSERT_FRAME: return "ASSERT_FRAME";
    case BIND_FRAME: return "BIND_FRAME";
    case BIND_LOCAL_VAR_FRAME: return "BIND_LOCAL_VAR_FRAME";
    case BIND_PROPAGATION_FRAME: return "BIND_PROPAGATION_FRAME";
    case EVAL_ARGS_FRAME: return "EVAL_ARGS_FRAME";
    case EVAL_FRAME: return "EVAL_FRAME";
    case EVAL_LOCAL_VAR_FRAME: return "EVAL_LOCAL_VAR_FRAME";
    case EVAL_SEQUENTIAL_FRAME: return "EVAL_SEQUENTIAL_FRAME";
    case FETCH_HANDLER_FRAME: return "FETCH_HANDLER_FRAME";
    case FETCH_OPERATOR_FRAME: return "FETCH_OPERATOR_FRAME";
    case HANDLER_FRAME: return "HANDLER_FRAME";
    case IF_FRAME: return "IF_FRAME";
    case LABELS_FRAME: return "LABELS_FRAME";
    case RETURN_ADDR_FRAME: return "RETURN_ADDR_FRAME";
    case RETURN_FRAME: return "RETURN_FRAME";
    case SWITCH_ENV_FRAME: return "SWITCH_ENV_FRAME";
    case THROW_FRAME: return "THROW_FRAME";
    default: xassert(FALSE); return NULL;
  }
}

static struct frame *alloc_frame(int type)
{
  struct frame *f;
  f = xmalloc(sizeof(struct frame) + sizeof(object) * frame_size(type));
  f->type = type;
  return f;
}

static struct frame *alloc_frame1(int type, object o)
{
  struct frame *f;
  f = alloc_frame(type);
  f->local_vars[0] = o;
  return f;
}

static struct frame *alloc_frame2(int type, object o, object p)
{
  struct frame *f;
  f = alloc_frame(type);
  f->local_vars[0] = o;
  f->local_vars[1] = p;
  return f;
}

static struct frame *fs_top(void)
{
  xassert(sp > 0);
  return fs[sp - 1];
}

static void fs_push(struct frame *f)
{
  if (sp > FRAME_STACK_SIZE - STACK_GAP) ip_mark_error("stack over flow");
  else if (sp + 3 > FRAME_STACK_SIZE) xerror("stack over flow.");
  fs[sp] = f;
  sp++;
}

static struct frame *fs_pop(void)
{
  struct frame *top;
  top = fs_top();
  --sp;
  return top;
}

static void pop_switch_env(void);
static void pop_return_addr_frame(void);
static void fs_rewind_pop(void)
{
  switch (fs_top()->type) {
    case SWITCH_ENV_FRAME: pop_switch_env(); break;
    case RETURN_ADDR_FRAME: pop_return_addr_frame(); break;
    default: fs_pop(); break;
  }
}

static void push_apply_frame(object operator)
{
  fs_push(alloc_frame1(APPLY_FRAME, operator));
}

static void push_apply_prim_frame(object prim)
{
  fs_push(alloc_frame1(APPLY_PRIM_FRAME, prim));
}

#ifndef NDEBUG
static void push_assert_frame(void)
{
  fs_push(alloc_frame(ASSERT_FRAME));
}
#endif

static void push_eval_frame(void)
{
  fs_push(alloc_frame(EVAL_FRAME));
}

static struct frame *make_bind_frame(object sym)
{
  return alloc_frame1(BIND_FRAME, sym);
}

static struct frame *make_bind_propagation_frame(object sym)
{
  return alloc_frame1(BIND_PROPAGATION_FRAME, sym);
}

static struct frame *make_eval_local_var_frame(object o)
{
  return alloc_frame1(EVAL_LOCAL_VAR_FRAME, o);
}

static void push_eval_args_frame(object args)
{
  if (args == object_nil) reg[0] = object_nil;
  else {
    fs_push(alloc_frame2(EVAL_ARGS_FRAME, args->cons.cdr, object_nil));
    push_eval_frame();
    reg[0] = args->cons.car;
  }
}

static void push_eval_sequential_frame(object args)
{
  if (args == object_nil) reg[0] = object_nil;
  else fs_push(alloc_frame1(EVAL_SEQUENTIAL_FRAME, args));
}

static void push_fetch_handler_frame(object body)
{
  fs_push(alloc_frame1(FETCH_HANDLER_FRAME, body));
}

static void push_fetch_operator_frame(object args)
{
  fs_push(alloc_frame1(FETCH_OPERATOR_FRAME, args));
}

static void push_handler_frame(object handler)
{
  fs_push(alloc_frame1(HANDLER_FRAME, handler));
}

static void push_if_frame(object args)
{
  if (args == object_nil) return;
  if (args->cons.cdr != object_nil) {
    fs_push(alloc_frame1(IF_FRAME, args->cons.cdr));
  }
  push_eval_frame();
  reg[0] = args->cons.car;
}

static struct frame *make_local_var_bind_frame(object sym, object val)
{
  return alloc_frame2(BIND_LOCAL_VAR_FRAME, sym, val);
}

static void push_labels_frame(object args)
{
  fs_push(alloc_frame1(LABELS_FRAME, args));
}

static void push_return_addr_frame(void)
{
  st_push(reg[3]);
  fs_push(alloc_frame(RETURN_ADDR_FRAME));
}

static void push_return_frame(void)
{
  fs_push(alloc_frame(RETURN_FRAME));
}

static void push_switch_env_frame(object env)
{
  fs_push(alloc_frame1(SWITCH_ENV_FRAME, reg[1]));
  reg[1] = gc_new_env(env);
}

static void push_throw_frame(void)
{
  fs_push(alloc_frame(THROW_FRAME));
}

static void parse_lambda_list(object env, object params, object args);
static void pop_apply_frame(void)
{
  object operator;
  operator = fs_pop()->local_vars[0];
  push_switch_env_frame(operator->lambda.env);
  fb_reset();
  parse_lambda_list(reg[1], operator->lambda.params, reg[0]);
  push_eval_sequential_frame(operator->lambda.body);
  fb_flush();
}

static void pop_apply_prim_frame(void)
{
  object args;
  int (*prim)(int, object, object *);
  args = reg[0];
  prim = xsplay_find(&prim_splay, fs_pop()->local_vars[0]);
  if ((*prim)(object_list_len(args), args, &(reg[0]))) return;
  if (error_msg == NULL) ip_mark_error("primitive failed");
}

#ifndef NDEBUG
static void pop_assert_frame(void)
{
  fs_pop();
  if (reg[0] == object_nil) ip_mark_error("assert failed");
}
#endif

static void pop_bind_frame(void)
{
  symbol_bind(reg[1], fs_pop()->local_vars[0], reg[0]);
}

static void pop_bind_local_var_frame(void)
{
  object s, v;
  struct frame *top;
  top = fs_pop();
  s = top->local_vars[0];
  v = top->local_vars[1];
  reg[0] = v;
  symbol_bind(reg[1], s, v);
}

static void pop_bind_propagation_frame(void)
{
  symbol_bind_propagation(reg[1], fs_pop()->local_vars[0], reg[0]);
}

static void pop_eval_frame(void)
{
  object s;
  fs_pop();
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
      push_fetch_operator_frame(reg[0]->cons.cdr);
      push_eval_frame();
      reg[3] = reg[0];
      reg[0] = reg[0]->cons.car;
      return;
    default: xassert(FALSE);
  }
}

static void pop_eval_local_var_frame(void)
{
  reg[0] = fs_top()->local_vars[0];
  pop_eval_frame();
}

static void pop_fetch_handler_frame(void)
{
  object handler, body;
  handler = reg[0];
  body = fs_pop()->local_vars[0];
  if (!typep(handler, LAMBDA)) {
    ip_mark_error("require exception handler");
    return;
  }
  if (object_list_len(handler->lambda.params) != 1) {
    ip_mark_error("handler parameter must be one required parameter");
    return;
  }
  push_handler_frame(reg[0]);
  push_eval_sequential_frame(body);
}

static void pop_fetch_operator_frame(void)
{
  object args;
  int (*special)(int, object);
  args = fs_pop()->local_vars[0];
  push_return_addr_frame();
  switch (type(reg[0])) {
    case SYMBOL:
      if ((special = xsplay_find(&special_splay, reg[0])) != NULL) {
        (*special)(object_list_len(args), args);
        return;
      }
      if (xsplay_find(&prim_splay, reg[0]) != NULL) {
        push_apply_prim_frame(reg[0]);
        push_eval_args_frame(args);
        return;
      }
      break;
    case MACRO:
      push_eval_frame();
      push_apply_frame(reg[0]);
      reg[0] = args;
      return;
    case LAMBDA:
      push_apply_frame(reg[0]);
      push_eval_args_frame(args);
      return;
    default: break;
  }
  ip_mark_error("is not a operator");
}

static void pop_eval_args_frame(void)
{
  struct frame *top;
  object rest, acc;
  top = fs_top();
  rest = top->local_vars[0];
  acc = top->local_vars[1] = gc_new_cons(reg[0], top->local_vars[1]);
  if (rest == object_nil) {
    fs_pop();
    reg[0] = object_reverse(acc);
  } else {
    top->local_vars[0] = rest->cons.cdr;
    push_eval_frame();
    reg[0] = rest->cons.car;
  }
}

static void pop_eval_sequential_frame(void)
{
  object args;
  args = fs_top()->local_vars[0];
  if (args == object_nil) fs_pop();
  else {
    fs_top()->local_vars[0] = args->cons.cdr;
    push_eval_frame();
    reg[0] = args->cons.car;
  }
}

static void pop_if_frame(void)
{
  object args;
  args = fs_pop()->local_vars[0];
  if (reg[0] != object_nil) {
    push_eval_frame();
    reg[0] = args->cons.car;
  } else if ((args = args->cons.cdr) != object_nil) push_if_frame(args);
}

static void pop_return_addr_frame(void)
{
  fs_pop();
  st_pop();
}

static void pop_switch_env(void)
{
  reg[1] = fs_pop()->local_vars[0];
}

static void pop_return_frame(void)
{
  while (sp != 0) {
    if (fs_top()->type == RETURN_ADDR_FRAME) return;
    fs_rewind_pop();
  }
}

static void pop_throw_frame(void)
{
  object handler, rewind_st;
  int rewind_rp;
  reg[2] = reg[0];
  rewind_rp = sp;
  rewind_st = symbol_find(object_toplevel, object_st);
  while (TRUE) {
    if (sp == 0) {
      sp = rewind_rp;
      symbol_bind(object_toplevel, object_st, rewind_st);
      ip_mark_error(NULL);
      return;
    }
    if (fs_top()->type == HANDLER_FRAME) {
      handler = fs_top()->local_vars[0];
      break;
    }
    fs_rewind_pop();
  }
  fs_pop();    // skip HANDLER_FRAME
  push_return_addr_frame();
  push_apply_frame(handler);
  reg[0] = gc_new_cons(reg[2], object_nil);
}

// validation etc.

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
    if (!typep(params->cons.car, CONS)) 
      fb_add(make_local_var_bind_frame(params->cons.car, args->cons.car));
    else {
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
        fb_add(make_local_var_bind_frame(sup_k, v));
      }
      if (args != object_nil) {
        fb_add(make_local_var_bind_frame(k, args->cons.car));
        args = args->cons.cdr;
      } else {
        if (def_v == NULL) fb_add(make_local_var_bind_frame(k, object_nil));
        else {
          fb_add(make_eval_local_var_frame(def_v));
          fb_add(make_bind_frame(k));
        }
      }
    }
  }
  // parse rest parameter
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    fb_add(make_local_var_bind_frame(params->cons.car, args));
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
      if (sup_k != NULL)
        fb_add(make_local_var_bind_frame(sup_k, object_bool(v != NULL)));
      if (v != NULL) fb_add(make_local_var_bind_frame(k, v));
      else {
        if (def_v == NULL) fb_add(make_local_var_bind_frame(k, object_nil));
        else {
          fb_add(make_eval_local_var_frame(def_v));
          fb_add(make_bind_frame(k));
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
  object params, s;
  if (!ip_ensure_arguments(argc, 1, FALSE)) return FALSE;
  if (!listp((params = argv->cons.car))) {
    ip_mark_error("argument must be list");
    return FALSE;
  }
  push_switch_env_frame(reg[1]);
  push_eval_sequential_frame(argv->cons.cdr);
  fb_reset();
  while (params != object_nil) {
    if (!typep((s = params->cons.car), SYMBOL)) {
      ip_mark_error("argument must be symbol");
      return FALSE;
    }
    if ((params = params->cons.cdr) == object_nil) {
      ip_mark_error("argument must be association list");
      return FALSE;
    }
    fb_add(make_eval_local_var_frame(params->cons.car));
    fb_add(make_bind_frame(s));
    params = params->cons.cdr;
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
      if (fs[i]->type == SWITCH_ENV_FRAME) {
        e = fs[i]->local_vars[0];
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
    fb_add(make_eval_local_var_frame(argv->cons.car));
    fb_add(make_bind_propagation_frame(s));
    argv = argv->cons.cdr;
    argc -= 2;
  }
  fb_flush();
  return TRUE;
}

SPECIAL(begin)
{
  push_eval_sequential_frame(argv);
  return TRUE;
}

SPECIAL(macro)
{
  object params;
  if (!ip_ensure_arguments(argc, 2, FALSE)) return FALSE;
  if (typep(argv->cons.car, SYMBOL)) {
    fs_push(make_bind_propagation_frame(argv->cons.car));
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
  push_if_frame(argv);
  return TRUE;
}

SPECIAL(labels)
{
  push_labels_frame(argv);
  push_eval_sequential_frame(argv);
  return TRUE;
}

SPECIAL(goto)
{
  object o;
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  reg[0] = argv->cons.car;
  if (!typep(reg[0], KEYWORD)) {
    ip_mark_error("arguments must be keyword");
    return FALSE;
  }
  while (fs_top()->type != LABELS_FRAME) {
    if (sp == 0) {
      ip_mark_error("not found labels context");
      return FALSE;
    }
    fs_rewind_pop();
  }
  o = fs_top()->local_vars[0];
  while (TRUE) {
    if (o == object_nil) {
      ip_mark_error("not found label");
      return FALSE;
    }
    if (o->cons.car == reg[0]) break;
    o = o->cons.cdr;
  }
  push_eval_sequential_frame(o);
  return TRUE;
}

SPECIAL(throw)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  push_throw_frame();
  push_eval_frame();
  reg[0] = argv->cons.car;
  return TRUE;
}

SPECIAL(catch)
{
  if (!ip_ensure_arguments(argc, 2, FALSE)) return FALSE;
  push_fetch_handler_frame(argv->cons.cdr);
  push_eval_frame();
  reg[0] = argv->cons.car;
  return TRUE;
}

SPECIAL(return)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  push_return_frame();
  push_eval_frame();
  reg[0] = argv->cons.car;
  return TRUE;
}

SPECIAL(assert)
{
#ifndef NDEBUG
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  push_assert_frame();
  push_eval_frame();
  reg[0] = argv->cons.car;
#endif
  return TRUE;
}

PRIM(eval)
{
  if (!ip_ensure_arguments(argc, 1, 1)) return FALSE;
  push_eval_frame();
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
        push_apply_prim_frame(argv->cons.car);
        reg[0] = argv->cons.cdr->cons.car;
        return TRUE;
      }
    case LAMBDA:
      push_apply_frame(argv->cons.car);
      reg[0] = argv->cons.cdr->cons.car;
      return TRUE;
    default: break;
  }
  ip_mark_error("requires function or symbol(built in function) to apply");
  return FALSE;
}

// trace and debug

static void sweep_env(int depth, void *sym, void *val)
{
  char buf[MAX_STR_LEN];
  printf(" (%s", object_describe(sym, buf));
  printf(" %s)", object_describe(val, buf));
}

static void describe_st(void)
{
  object st, e, msg;
  char buf[MAX_STR_LEN];
  if (error_msg != NULL) {
    e = object_error;
    msg = gc_new_barray_from(STRING, strlen(error_msg), error_msg);
  } else  {
    xassert(typep(reg[2], CONS));
    xassert(typep(reg[2]->cons.cdr, CONS));
    xassert(typep(reg[2]->cons.cdr->cons.cdr, CONS));
    xassert(typep(reg[2]->cons.cdr->cons.cdr->cons.cdr, CONS));
    e = reg[2]->cons.cdr->cons.car;
    msg = reg[2]->cons.cdr->cons.cdr->cons.cdr->cons.car;
  }
  printf("%s", object_describe(e, buf));
  if (msg != object_nil)
    printf(" -- %s.\n", object_describe(msg, buf));
  st = symbol_find(object_toplevel, object_st);
  while (st != object_nil) {
    printf("	at: %s\n", object_describe(st->cons.car, buf));
    st = st->cons.cdr;
  }
}

void describe_reg(void)
{
  int i;
  char buf[MAX_STR_LEN];
  printf("; register\n");
  printf("(");
  for (i = 0; i < REG_SIZE; i++) {
    if (i != 0) printf(" ");
    printf("%s", object_describe(reg[i], buf));
  }
  printf(")\n");
}

void describe_env(void)
{
  object e;
  printf("; environment\n");
  e = reg[1];
  while (e != object_nil) {
    xassert(typep(e, ENV));
    printf("(<%p>", e);
    xsplay_foreach(&e->env.binding, sweep_env);
    printf(")\n");
    e = e->env.top;
  }
}

void describe_fs(void)
{
  int i, j;
  char buf[MAX_STR_LEN];
  struct frame *f;
  printf("; frame stack\n");
  for (i = sp - 1; i >= 0; i--) {
    f = fs[i];
    printf("(%s", frame_name(f->type));
    for (j = 0; j < frame_size(f->type); j++)
      printf(" %s", object_describe(f->local_vars[j], buf));
    printf(")\n");
  }
  printf("\n");
}

static void trap(void)
{
  switch (ip_trap_code) {
    case TRAP_ERROR:
      // describe_vm();
      describe_st();
      exit(1);
      break;
    default:
      ip_trap_code=TRAP_NONE;
      break;
  }
}

static object eval(object expr)
{
  xassert(sp == 0);
  reg[0] = expr;
  reg[1] = object_toplevel;
  reg[2] = object_nil;
  reg[3] = object_nil;
  push_eval_frame();
  while (TRUE) {
    xassert(sp >= 0);
    if (ip_trap_code != TRAP_NONE) trap();
    if (sp == 0) break;
    switch (fs_top()->type) {
      case APPLY_FRAME: pop_apply_frame(); break;
      case APPLY_PRIM_FRAME: pop_apply_prim_frame(); break;
#ifndef NDEBUG
      case ASSERT_FRAME: pop_assert_frame(); break;
#endif
      case BIND_FRAME: pop_bind_frame(); break;
      case BIND_LOCAL_VAR_FRAME: pop_bind_local_var_frame(); break;
      case BIND_PROPAGATION_FRAME: pop_bind_propagation_frame(); break;
      case EVAL_FRAME: pop_eval_frame(); break;
      case EVAL_LOCAL_VAR_FRAME: pop_eval_local_var_frame(); break;
      case EVAL_ARGS_FRAME: pop_eval_args_frame(); break;
      case EVAL_SEQUENTIAL_FRAME: pop_eval_sequential_frame(); break;
      case FETCH_HANDLER_FRAME: pop_fetch_handler_frame(); break;
      case FETCH_OPERATOR_FRAME: pop_fetch_operator_frame(); break;
      case HANDLER_FRAME: fs_pop(); break;
      case IF_FRAME: pop_if_frame(); break;
      case LABELS_FRAME: fs_pop(); break;
      case RETURN_FRAME: pop_return_frame(); break;
      case RETURN_ADDR_FRAME: pop_return_addr_frame(); break;
      case SWITCH_ENV_FRAME: pop_switch_env(); break;
      case THROW_FRAME: pop_throw_frame(); break;
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
  char buf[MAX_STR_LEN];
  object o, p;
  cycle = sp = 0;
  xarray_init(&fb);
  ip_trap_code = TRAP_NONE;
  for (o = object_boot->lambda.body; o != object_nil; o = o->cons.cdr) {
    p = eval(o->cons.car);
    if (VERBOSE_P) printf("%s\n", object_describe(p, buf));
  }
}
