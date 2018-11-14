// interpreter

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

/*
 * registers:
 *   0 -- frame argument.
 *   1 -- current environment.
 */
#define REG_SIZE 2
static object reg[REG_SIZE];

static long cycle;

// error

int ip_trap_code;
static char *error_message;
static void describe_vm(void);

static void mark_error(char *msg)
{
  ip_trap_code = TRAP_ERROR;
  error_message = msg;
}

// gloval environment

static struct xsplay special_splay;
static struct xsplay prim_splay;

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
#define BIND_FRAME 2
#define BIND_LOCAL_VAR_FRAME 3
#define BIND_PROPAGATION_FRAME 4
#define CATCH_FRAME 5
#define EVAL_FRAME 6
#define EVAL_LOCAL_VAR_FRAME 7
#define EVAL_ARGS_FRAME 8
#define EVAL_SEQUENTIAL_FRAME 9
#define FETCH_OPERATOR_FRAME 10
#define IF_FRAME 11
#define LABELS_FRAME 12
#define RETURN_ADDR_FRAME 13
#define RETURN_FRAME 14
#define SWITCH_ENV_FRAME 15
#define THROW_FRAME 16
#define TRY_FRAME 17
  object local_vars[0];
};

static int frame_size(int type)
{
  switch (type) {
    case EVAL_FRAME:
    case RETURN_ADDR_FRAME:
    case RETURN_FRAME:
    case THROW_FRAME:
      return 0;
    case APPLY_FRAME:
    case APPLY_PRIM_FRAME:
    case BIND_FRAME:
    case BIND_PROPAGATION_FRAME:
    case EVAL_SEQUENTIAL_FRAME:
    case FETCH_OPERATOR_FRAME:
    case EVAL_LOCAL_VAR_FRAME:
    case IF_FRAME:
    case LABELS_FRAME:
    case SWITCH_ENV_FRAME:
    case TRY_FRAME:
      return 1;
    case BIND_LOCAL_VAR_FRAME:
    case EVAL_ARGS_FRAME:
      return 2;
    case CATCH_FRAME:
      return 3;
    default: xassert(FALSE);
  }
  return FALSE;
}

static char *frame_name(int type)
{
  switch (type) {
    case APPLY_FRAME: return "APPLY_FRAME";
    case APPLY_PRIM_FRAME: return "APPLY_PRIM_FRAME";
    case BIND_FRAME: return "BIND_FRAME";
    case BIND_LOCAL_VAR_FRAME: return "BIND_LOCAL_VAR_FRAME";
    case BIND_PROPAGATION_FRAME: return "BIND_PROPAGATION_FRAME";
    case CATCH_FRAME: return "CATCH_FRAME";
    case EVAL_FRAME: return "EVAL_FRAME";
    case EVAL_LOCAL_VAR_FRAME: return "EVAL_LOCAL_VAR_FRAME";
    case EVAL_ARGS_FRAME: return "EVAL_ARGS_FRAME";
    case EVAL_SEQUENTIAL_FRAME: return "EVAL_SEQUENTIAL_FRAME";
    case FETCH_OPERATOR_FRAME: return "FETCH_OPERATOR_FRAME";
    case IF_FRAME: return "IF_FRAME";
    case LABELS_FRAME: return "LABELS_FRAME";
    case RETURN_ADDR_FRAME: return "RETURN_ADDR_FRAME";
    case RETURN_FRAME: return "RETURN_FRAME";
    case SWITCH_ENV_FRAME: return "SWITCH_ENV_FRAME";
    case TRY_FRAME: return "TRY_FRAME";
    default: xassert(FALSE);
  }
  return NULL;
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
  if (sp > FRAME_STACK_SIZE - STACK_GAP) mark_error("stack over flow.");
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
static void fs_rewind_pop(void)
{
  switch (fs_top()->type) {
    case SWITCH_ENV_FRAME: pop_switch_env(); break;
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

static void push_eval_frame(void)
{
  fs_push(alloc_frame(EVAL_FRAME));
}

static struct frame *make_bind_frame(object sym)
{
  return alloc_frame1(BIND_FRAME, sym);
}

static struct frame *make_local_var_bind_frame(object sym, object val)
{
  return alloc_frame2(BIND_LOCAL_VAR_FRAME, sym, val);
}

static struct frame *make_bind_propagation_frame(object sym)
{
  return alloc_frame1(BIND_PROPAGATION_FRAME, sym);
}

static struct frame *make_catch_frame(object e, object s, object body)
{
  struct frame *f;
  f = alloc_frame(CATCH_FRAME);
  f->local_vars[0] = e;
  f->local_vars[1] = s;
  f->local_vars[2] = body;
  return f;
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

static void push_fetch_operator_frame(object args)
{
  fs_push(alloc_frame1(FETCH_OPERATOR_FRAME, args));
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

static void push_labels_frame(object args)
{
  fs_push(alloc_frame1(LABELS_FRAME, args));
}

static void push_return_addr_frame(void)
{
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

static void push_throw_frame(object e)
{
  fs_push(alloc_frame1(THROW_FRAME, e));
}

static void push_try_frame(object f)
{
  fs_push(alloc_frame1(TRY_FRAME, f));
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
  if (!(*prim)(object_length(args), args, &(reg[0])))
    mark_error("primitive failed");
}

static void pop_bind_frame(void)
{
  xsplay_replace(&reg[1]->env.binding, fs_pop()->local_vars[0], reg[0]);
}

static void pop_bind_local_var_frame(void)
{
  object s, v;
  struct frame *top;
  top = fs_pop();
  s = top->local_vars[0];
  v = top->local_vars[1];
  reg[0] = v;
  xsplay_replace(&reg[1]->env.binding, s, v);
}

static void pop_bind_propagation_frame(void)
{
  object e, s;
  e = reg[1];
  s = fs_pop()->local_vars[0];
  while (e != object_toplevel) {
    xassert(typep(e, ENV));
    if (xsplay_find(&e->env.binding, s) != NULL) {
      xsplay_replace(&e->env.binding, s, reg[0]);
      return;
    }
    e = e->env.top;
  }
  xsplay_replace(&object_toplevel->env.binding, s, reg[0]);
}

static object symbol_find(void)
{
  object e, v;
  e = reg[1];
  while (e != object_nil) {
    if ((v = xsplay_find(&e->env.binding, reg[0])) != NULL) break;
    e = e->env.top;
  }
  return v;
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
    case BARRAY:
      return;
    case SYMBOL:
      if ((s = symbol_find()) == NULL) mark_error("unbind symbol");
      else reg[0] = s;
      return;
    case CONS:
      push_fetch_operator_frame(reg[0]->cons.cdr);
      push_eval_frame();
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

static void pop_fetch_operator_frame(void)
{
  object args;
  void (*special)(object, int, object);
  args = fs_pop()->local_vars[0];
  switch (type(reg[0])) {
    case SYMBOL:
      if ((special = xsplay_find(&special_splay, reg[0])) != NULL) {
        (*special)(reg[1], object_length(args), args);
        return;
      }
      if (xsplay_find(&prim_splay, reg[0]) != NULL) {
        push_apply_prim_frame(reg[0]);
        push_eval_args_frame(args);
        return;
      }
      break;
    case MACRO:
      push_return_addr_frame();
      push_eval_frame();
      push_apply_frame(reg[0]);
      reg[0] = args;
      return;
    case LAMBDA:
      push_return_addr_frame();
      push_apply_frame(reg[0]);
      push_eval_args_frame(args);
      return;
    default: break;
  }
  mark_error("is not a operator");
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
  int rp;
  object te, targ, ce, cs, cbody;
  te = fs_pop()->local_vars[0];
  targ = reg[0];
  rp = sp;
  while (TRUE) {
    if (sp == 0) {
      sp = rp;
      mark_error("error");
      return;
    }
    if (fs_top()->type == CATCH_FRAME) {
      ce = fs_top()->local_vars[0];
      cs = fs_top()->local_vars[1];
      cbody = fs_top()->local_vars[2];
      while (ce != object_nil) {
        if (te == ce->cons.car) break;
        ce = ce->cons.cdr;
      }
      if (ce != object_nil) {
        while (fs_top()->type != TRY_FRAME) fs_rewind_pop();
        break;
      }
    }
    fs_rewind_pop();
  }
  fs_pop();    // skip TRY_FRAME
  if (cs != object_nil) push_switch_env_frame(reg[1]);
  push_eval_sequential_frame(cbody);
  if (cs != object_nil) fs_push(make_local_var_bind_frame(cs, targ));
}

// validation etc.

static int valid_keyword_p(object params, object args)
{
  object p, s;
  while (args != object_nil) {
    if (!typep(args->cons.car, KEYWORD)) {
      mark_error("expected keyword parameter");
      return FALSE;
    }
    p = params;
    while (p != object_nil) {
      s = p->cons.car;
      if (typep(s, CONS)) s = s->cons.car;
      xassert(typep(s, SYMBOL));
      if (strcmp(s->symbol.name, args->cons.car->symbol.name + 1) == 0)
        break;
      p = p->cons.cdr;
    }
    if (p == object_nil) {
      mark_error("undeclared keyword parameter");
      return FALSE;
    }
    if ((args = args->cons.cdr) == object_nil) {
      mark_error("expected keyword parameter value");
      return FALSE;
    }
    args = args->cons.cdr;
  }
  return TRUE;
}

// lambda-list内のシンボルが一意であることのvalidate-lambda-listに追加する。
static void parse_lambda_list(object env, object params, object args)
{
  object o, pre, k, v, def_v, sup_k;
  // parse required parameter
  while (params != object_nil && !typep(params->cons.car, KEYWORD)) {
    if (args == object_nil) {
      mark_error("too few arguments");
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
    if(!valid_keyword_p(params, args)) return;
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
        if (strcmp((o->cons.car->symbol.name + 1), k->symbol.name) == 0) {
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
  if (args != object_nil) mark_error("too many arguments");
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

static int valid_lambda_list_p(int lambda_type, object params)
{
  int type;
  while (TRUE) {
    if (params == object_nil) return TRUE;
    type = type(params->cons.car);
    if (type == KEYWORD) break;
    else if (type == SYMBOL) params = params->cons.cdr;
    else if (type == CONS) {
      if (lambda_type != MACRO || !valid_lambda_list_p(MACRO, params->cons.car))
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
  if (argc < 1) {
    mark_error("let: too few argument");
    return;
  }
  if (!listp((params = argv->cons.car))) {
    mark_error("let: argument must be list");
    return;
  }
  push_switch_env_frame(env);
  push_eval_sequential_frame(argv->cons.cdr);
  fb_reset();
  while (params != object_nil) {
    if (!typep((s = params->cons.car), SYMBOL)) {
      mark_error("let: argument must be symbol");
      return;
    }
    if ((params = params->cons.cdr) == object_nil) {
      mark_error("let: argument must be association list");
      return;
    }
    fb_add(make_eval_local_var_frame(params->cons.car));
    fb_add(make_bind_frame(s));
    params = params->cons.cdr;
  }
  fb_flush();
}

// <assign> ::= (<- [<sym> <val>] ...)
// <sym> -- symbol
// <val> -- value
SPECIAL(assign)
{
  object s;
  if (argc == 0) return;
  if (argc % 2 != 0) {
    mark_error("<-: must be pair");
    return;
  }
  fb_reset();
  while (argc != 0) {
    s = argv->cons.car;
    if (!typep(s, SYMBOL)) {
      mark_error("<-: cannot bind except symbol");
      return;
    }
    if (s == object_nil) {
      mark_error("<-: cannot bind nil");
      return;
    }
    argv = argv->cons.cdr;
    fb_add(make_eval_local_var_frame(argv->cons.car));
    fb_add(make_bind_propagation_frame(s));
    argv = argv->cons.cdr;
    argc -= 2;
  }
  fb_flush();
}

SPECIAL(begin)
{
  push_eval_sequential_frame(argv);
}

SPECIAL(macro)
{
  object params;
  if (typep(argv->cons.car, SYMBOL)) {
    fs_push(make_bind_frame(argv->cons.car));
    argv = argv->cons.cdr;
  }
  if (!valid_lambda_list_p(MACRO, params = argv->cons.car)) {
    mark_error("macro: illegal parameter list");
    return;
  }
  reg[0] = gc_new_macro(env, params, argv->cons.cdr);
}

SPECIAL(lambda)
{
  object params;
  params = argv->cons.car;
  if (!valid_lambda_list_p(LAMBDA, params)) {
    mark_error("lambda: illegal parameter list");
    return;
  }
  reg[0] = gc_new_lambda(env, params, argv->cons.cdr);
}

SPECIAL(quote)
{
  if (argc != 1) {
    if (argc == 0) mark_error("quote: requires argument");
    else mark_error("quote: too many arguments");
    return;
  }
  reg[0] = argv->cons.car;
}

SPECIAL(if)
{
  if (argc < 2) {
    mark_error("if: too few arguments");
    return;
  }
  push_if_frame(argv);
}

SPECIAL(labels)
{
  push_labels_frame(argv);
  push_eval_sequential_frame(argv);
}

SPECIAL(goto)
{
  object o;
  if (argc != 1) {
    if (argc == 0) mark_error("goto: requires arguments");
    else mark_error("goto: too many arguments");
    return;
  }
  reg[0] = argv->cons.car;
  if (!typep(reg[0], KEYWORD)) {
    mark_error("goto: arguments must be keyword");
    return;
  }
  while (fs_top()->type != LABELS_FRAME) {
    if (sp == 0) {
      mark_error("goto: not found labels context");
      return;
    }
    fs_rewind_pop();
  }
  o = fs_top()->local_vars[0];
  while (TRUE) {
    if (o == object_nil) {
      mark_error("goto: not found label");
      return;
    }
    if (o->cons.car == reg[0]) break;
    o = o->cons.cdr;
  }
  push_eval_sequential_frame(o);
}

SPECIAL(throw)
{
  if (argc == 0 || argc > 2) {
    if (argc == 0) mark_error("throw: too few arguments");
    else mark_error("throw: too many arguments");
    return;
  }
  if (!typep(argv->cons.car, KEYWORD)) {
    mark_error("throw: type must be keyword");
    return;
  }
  push_throw_frame(argv->cons.car);
  if (argc == 1) reg[0] = object_exception;
  else {
    push_eval_frame();
    reg[0] = argv->cons.cdr->cons.car;
  }
}

SPECIAL(try)
{
  object p, s, params, cparams, finally;
  if (argc < 1) {
    mark_error("try: too few argument");
    return;
  }
  if (!listp((params = argv->cons.car))) {
    mark_error("try: argument must be list");
    return;
  }
  finally = NULL;
  fb_reset();
  while (params != object_nil) {
    p = params->cons.car;
    if (!typep(p, CONS)) {
      mark_error("try: illegal arguments");
      return;
    }
    if (p->cons.car == object_catch) {
      s = object_nil;
      p = p->cons.cdr;
      if (!typep((cparams = p->cons.car), CONS)) {
        mark_error("try: illegal catch clause");
        return;
      }
      while (cparams != object_nil) {
        if (typep(cparams->cons.car, KEYWORD)) {
          cparams = cparams->cons.cdr;
          continue;
        }
        if (typep(cparams->cons.car, SYMBOL) && cparams->cons.cdr == object_nil)
        {
          s = cparams->cons.car;
          break;
        }
        mark_error("try: illegal parameter list");
        return;
      }
      fb_add(make_catch_frame(p->cons.car, s, p->cons.cdr));
    } else if (p->cons.car == object_finally) {
      if (finally != NULL) {
        mark_error("try: too many finally clause");
        return;
      }
      finally = p->cons.cdr;
    } else {
      mark_error("try: illegal arguments");
      return;
    }
    params = params->cons.cdr;
  }
  if (finally == NULL) finally = object_nil;
  push_try_frame(finally);
  fb_flush();
  push_eval_sequential_frame(argv->cons.cdr);
}

SPECIAL(return)
{
  if (argc != 1) {
    mark_error("return: illegal arguments");
    return;
  }
  push_return_frame();
  push_eval_frame();
  reg[0] = argv->cons.car;
}

// TODO should be removed
SPECIAL(vm)
{
  describe_vm();
}

// TODO should be removed
SPECIAL(break)
{
  printf("%s\n", "break");
}

// trace and debug

static void sweep_env(int depth, void *sym, void *val)
{
  char buf[MAX_STR_LEN];
  printf(" (%s", object_describe(sym, buf));
  printf(" %s)", object_describe(val, buf));
}

static void describe_reg(void)
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

static void describe_env(void)
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

static void describe_fs(void)
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

static void describe_vm(void)
{
  describe_reg();
  describe_env();
  describe_fs();
}

static void trap(void)
{
  if(ip_trap_code == TRAP_ERROR) {
    describe_vm();
    xerror("%s\n", error_message);
  }
  ip_trap_code=TRAP_NONE;
}

static object eval(object expr)
{
  xassert(sp == 0);
  reg[0] = expr;
  reg[1] = object_toplevel;
  push_eval_frame();
  while (TRUE) {
    xassert(sp >= 0);
    if (ip_trap_code != TRAP_NONE) trap();
    if (sp == 0) break;
    switch (fs_top()->type) {
      case APPLY_FRAME: pop_apply_frame(); break;
      case APPLY_PRIM_FRAME: pop_apply_prim_frame(); break;
      case BIND_FRAME: pop_bind_frame(); break;
      case BIND_LOCAL_VAR_FRAME: pop_bind_local_var_frame(); break;
      case BIND_PROPAGATION_FRAME: pop_bind_propagation_frame(); break;
      case CATCH_FRAME: fs_pop(); break;
      case EVAL_FRAME: pop_eval_frame(); break;
      case EVAL_LOCAL_VAR_FRAME: pop_eval_local_var_frame(); break;
      case EVAL_ARGS_FRAME: pop_eval_args_frame(); break;
      case EVAL_SEQUENTIAL_FRAME: pop_eval_sequential_frame(); break;
      case FETCH_OPERATOR_FRAME: pop_fetch_operator_frame(); break;
      case IF_FRAME: pop_if_frame(); break;
      case LABELS_FRAME: fs_pop(); break;
      case RETURN_FRAME: pop_return_frame(); break;
      case RETURN_ADDR_FRAME: fs_pop(); break;
      case SWITCH_ENV_FRAME: pop_switch_env(); break;
      case THROW_FRAME: pop_throw_frame(); break;
      case TRY_FRAME: fs_pop(); break;
      default: xassert(FALSE);
    }
  }
  return reg[0];
}

void ip_mark(void)
{
  // TODO mark fs, regs, ....
}

static void init_builtin(void)
{
  int i;
  char *s;
  xsplay_init(&special_splay, (int(*)(void *, void *))symcmp);
  xsplay_init(&prim_splay, (int(*)(void *, void *))symcmp);
  for (i = 0; (s = bi_as_symbol_name(special_name_table[i])) != NULL; i++)
    xsplay_add(&special_splay, gc_new_symbol(s), special_table[i]);
  for (i = 0; (s = bi_as_symbol_name(prim_name_table[i])) != NULL; i++)
    xsplay_add(&prim_splay, gc_new_symbol(s), prim_table[i]);
}

void ip_start(void)
{
  char buf[MAX_STR_LEN];
  object o, p;
  init_builtin();
  cycle = sp = 0;
  xarray_init(&fb);
  ip_trap_code = TRAP_NONE;
  for (o = object_boot->lambda.body; o != object_nil; o = o->cons.cdr) {
    p = eval(o->cons.car);
    if (VERBOSE_P) printf("%s\n", object_describe(p, buf));
  }
}
