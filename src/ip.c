// interpreter

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static object reg[3]; // general-purpose register.

static struct xsplay special_splay;
static struct xsplay prim_splay;

// error
int ip_trap_code;
static char *error_message;

static void mark_error(char *msg)
{
  ip_trap_code = TRAP_ERROR;
  error_message = msg;
}

// frame stack

/*
 * (+ 1 (- 2 3))
 * eval
 * eval-args[(1 (- 2 3)), nil], apply[+]
 * eval-args[((- 2 3)), (1)], apply[+]
 * eval-args[(2 3)], apply[-], eval-args[((- 2 3)), (1)], apply[+]
 * eval-args[(3) (2)], apply[-], eval-args[((- 2 3)), (1)], apply[+]
 * eval-args[nil, (2 3)], apply[-], eval-args[((- 2 3)), (1)], apply[+]
 * eval-args[nil, (1 -1)], apply[+]
 * 0
 */

/*
 * each frame
 * eval-frame[env]
 * eval-operator-frame[env, operands]
 * eval-operands-frame[env, expr, acc]
 * apply-frame[env, operator]
 * eval-sequential-frame[env, rest]
 */

#define STACK_GAP 5
#define FRAME_STACK_SIZE 1000

// frame type
#define APPLY_PRIM_FRAME 0
#define EVAL_FRAME 1
#define EVAL_OPERANDS_FRAME 2
#define EVAL_OPERATOR_FRAME 3
#define EVAL_SEQUENTIAL_FRAME 4

struct frame {
  int type;
  object local_vars[1];
};

static int sp;
struct frame *fs[FRAME_STACK_SIZE];

static int frame_size(int type)
{
  switch (type) {
    case APPLY_PRIM_FRAME:
    case EVAL_FRAME:
      return 1;
    case EVAL_OPERATOR_FRAME:
      return 2;
    case EVAL_OPERANDS_FRAME:
      return 3;
    default: xassert(FALSE);
  }
  return FALSE;
}

static char *frame_name(int type)
{
  switch (type) {
    case APPLY_PRIM_FRAME: return "APPLY_PRIM_FRAME";
    case EVAL_FRAME: return "EVAL_FRAME";
    case EVAL_OPERANDS_FRAME: return "EVAL_OPERANDS_FRAME";
    case EVAL_OPERATOR_FRAME: return "EVAL_OPERATOR_FRAME";
    default: xassert(FALSE);
  }
  return NULL;
}

static struct frame *alloc_frame(int type)
{
  struct frame *f;
  f = xmalloc(sizeof(struct frame) + sizeof(object) * (frame_size(type) - 1));
  f->type = type;
  return f;
}

static struct frame *fs_top(void)
{
  xassert(sp > 0);
  return fs[sp - 1];
}

static void dump_fs(void)
{
  int i, j;
  char buf[MAX_STR_LEN];
  struct frame *f;
  printf("-- frame stack\n");
  for (i = sp - 1; i >= 0; i--) {
    f = fs[i];
    printf("%s[", frame_name(f->type));
    for (j = 0; j < frame_size(f->type); j++) {
      if (j != 0) printf(", ");
      printf("%s", object_describe(f->local_vars[j], buf));
    }
    printf("]\n");
  }
  printf("\n");
}

static void push_frame(struct frame *f)
{
  if (sp > FRAME_STACK_SIZE - STACK_GAP) mark_error("stack over flow.");
  else if (sp + 3 > FRAME_STACK_SIZE) xerror("stack over flow.");
  fs[sp] = f;
  sp++;
}

static struct frame *pop_frame(void)
{
  struct frame *top;
  top = fs_top();
  --sp;
  return top;
}

static struct frame *make_eval_frame(object env)
{
  struct frame *f;
  f = alloc_frame(EVAL_FRAME);
  f->local_vars[0] = env;
  return f;
}

static struct frame *make_eval_operator_frame(object env, object operands)
{
  struct frame *f;
  f = alloc_frame(EVAL_OPERATOR_FRAME);
  f->local_vars[0] = env;
  f->local_vars[1] = operands;
  return f;
}

static struct frame *make_eval_operands_frame(object env , object operands)
{
  struct frame *f;
  f = alloc_frame(EVAL_OPERANDS_FRAME);
  f->local_vars[0] = env;
  f->local_vars[1] = operands;
  f->local_vars[2] = object_nil;
  return f;
}

static struct frame *make_apply_prim_frame(object prim)
{
  struct frame *f;
  f = alloc_frame(APPLY_PRIM_FRAME);
  f->local_vars[0] = prim;
  return f;
}

void ip_mark(void)
{
  // TODO mark fs.
}

static object symbol_find(object e, object o)
{
  object result;
  while (e != object_nil) {
    if ((result = xsplay_find(&e->env.binding, o)) != NULL) break;
    e = e->env.top;
  }
  return result;
}

// evaluater
static object todo_eval(object env, object expr);

static object eval_operands(object env, object expr)
{
  object o;
  if (expr == object_nil) return object_nil;
  o = todo_eval(env, expr->cons.car);
  expr = expr->cons.cdr;
  return gc_new_cons(o, eval_operands(env, expr));
}

static object eval_sequential(object env, object expr)
{
  object o;
  xassert(listp(expr));
  while (expr != object_nil) {
    o = todo_eval(env, expr->cons.car);
    expr = expr->cons.cdr;
  }
  return o;
}

static void bind_lambda_list(object env, object params, object operands)
{
  int supply_p, rest_p;
  object o, k, v;
  struct xsplay s;
  rest_p = FALSE;
  while (params != object_nil && !typep(params->cons.car, Keyword)) {
    if (operands == object_nil) {
      mark_error("too few arguments");
      return;
    }
    if (typep(params->cons.car, Cons)) {
      bind_lambda_list(env, params->cons.car, operands->cons.car);
      if (ip_trap_code != TRAP_NONE) return;
    } else xsplay_add(&env->env.binding, params->cons.car, operands->cons.car);
    params = params->cons.cdr;
    operands = operands->cons.cdr;
  }
  if (params->cons.car == object_opt) {
    params = params->cons.cdr;
    while (params != object_nil && !typep(params->cons.car, Keyword)) {
      o = params->cons.car;
      supply_p = FALSE;
      if (!typep(o, Cons)) {
        k = o;
        v = object_nil;
      } else {
        k = o->cons.car;
        v = todo_eval(env, (o = o->cons.cdr)->cons.car);
        if ((o = o->cons.cdr) != object_nil) {
          supply_p = TRUE;
          xsplay_add(&env->env.binding, o->cons.car, object_nil);
        }
      }
      params = params->cons.cdr;
      if (operands != object_nil) {
        v = operands->cons.car;
        operands = operands->cons.cdr;
        if (supply_p)
          xsplay_replace(&env->env.binding, o->cons.car, object_true);
      }
      xsplay_add(&env->env.binding, k, v);
    }
  }
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    xsplay_add(&env->env.binding, params->cons.car, operands);
    params = params->cons.cdr;
    rest_p = TRUE;
  }
  if (params->cons.car == object_key) {
    params = params->cons.cdr;
    xsplay_init(&s, (int(*)(void *, void *))symcmp);
    while (params != object_nil) {
      o = params->cons.car;
      if (!typep(o, Cons)) {
        k = o;
        v = object_nil;
      } else {
        k = o->cons.car;
        v = todo_eval(env, (o = o->cons.cdr)->cons.car);
        if ((o = o->cons.cdr) != object_nil) {
          xsplay_add(&s, k, o->cons.car);
          xsplay_add(&env->env.binding, o->cons.car, object_nil);
        }
      }
      xsplay_add(&env->env.binding, k, v);
      params = params->cons.cdr;
    }
    while (operands != object_nil) {
      if (!typep(operands->cons.car, Keyword)) {
        mark_error("expected keyword parameter");
        return;
      }
      k = gc_new_symbol(operands->cons.car->symbol.name + 1);    // skip ':'
      if ((v = xsplay_find(&env->env.binding, k)) == NULL) {
        mark_error("undeclared keyword parameter");
        return;
      }
      operands = operands->cons.cdr;
      v = operands->cons.car;
      xsplay_replace(&env->env.binding, k, v);
      if ((o = xsplay_find(&s, k)) != NULL)
        xsplay_replace(&env->env.binding, o, object_true);
      operands = operands->cons.cdr;
    }
  }
  if (!rest_p && operands != object_nil) mark_error("too many arguments");
}

static object i_apply(object operator, object operands)
{
  object e;
  e = gc_new_env(operator->lambda.env);
  bind_lambda_list(e, operator->lambda.params, operands);
  return eval_sequential(e, operator->lambda.body);
}

// should be removed.
static object todo_eval(object env, object expr)
{
  int argc;
  object (*special)(object, int, object);
  object (*prim)(int, object, object *);
  object operator, operands, result;
  switch (type(expr)) {
    case Macro:
    case Lambda:
    case Xint:
    case Xfloat:
    case Keyword:
      result = expr;
      break;
    case Symbol:
      if ((result = symbol_find(env, expr)) == NULL)
        mark_error("unbind symbol");
      break;
    case Cons:
      operator = todo_eval(env, expr->cons.car);
      operands = expr->cons.cdr;
      argc = object_length(operands);
      result = NULL;
      switch (type(operator)) {
        case Symbol:
          if ((special = xsplay_find(&special_splay, operator)) != NULL) {
            result = (*special)(env, argc, operands);
          } else if ((prim = xsplay_find(&prim_splay, operator)) != NULL) {
            if (!(*prim)(argc, eval_operands(env, operands), &result))
              mark_error("primitive failed");
          }
          break;
        case Macro:
          result = todo_eval(env, i_apply(operator, operands));
          break;
        case Lambda:
          result = i_apply(operator, eval_operands(env, operands));
          break;
        default: break;
      }
      if (result == NULL) mark_error("is not a operator");
      break;
    default: xassert(FALSE);
  }
  return result;
}

static void pop_eval_frame(void)
{
  reg[1] = pop_frame()->local_vars[0];
  switch (type(reg[0])) {
    case Macro:
    case Lambda:
    case Xint:
    case Xfloat:
    case Keyword:
      return;
    case Symbol:
      reg[2] = reg[0];
      if ((reg[0] = symbol_find(reg[1], reg[0])) == NULL)
        mark_error("unbind symbol");
      return;
    case Cons:
      reg[2] = reg[0]->cons.cdr;
      push_frame(make_eval_operator_frame(reg[1], reg[2]));
      push_frame(make_eval_frame(reg[1]));
      reg[0] = reg[0]->cons.car;
      return;
  }
}

static void push_eval_operands_frame(object env, object operands)
{
  if (operands == object_nil) reg[0] = object_nil;
  else push_frame(make_eval_operands_frame(env, operands));
}

static void pop_eval_operator_frame(void)
{
  struct frame *top;
  object (*special)(object, int, object);
  top = pop_frame();
  reg[1] = top->local_vars[0];
  reg[2] = top->local_vars[1];
  switch (type(reg[0])) {
    case Symbol:
      if ((special = xsplay_find(&special_splay, reg[0])) != NULL) {
        reg[0] = (*special)(reg[1], object_length(reg[2]), reg[2]);
        return;
      }
      if (xsplay_find(&prim_splay, reg[0]) != NULL) {
        push_frame(make_apply_prim_frame(reg[0]));
        push_eval_operands_frame(reg[1], reg[2]);
        return;
      }
      break;
    case Macro:
      reg[0] = todo_eval(reg[1], i_apply(reg[0], reg[2]));
      return;
    case Lambda:
      reg[0] = i_apply(reg[0], eval_operands(reg[1], reg[2]));
      return;
    default: break;
  }
  mark_error("is not a operator");
}

static void pop_apply_prim_frame(void)
{
  struct frame *top;
  int (*prim)(int, object, object *);
  top = pop_frame();
  reg[1] = top->local_vars[0];
  reg[2] = reg[0];
  prim = xsplay_find(&prim_splay, reg[1]);
  if (!(*prim)(object_length(reg[2]), reg[2], &(reg[0]))) {
    mark_error("primitive failed");
    return;
  }
}

static void pop_eval_operands_frame(void)
{
  struct frame *top;
  top = pop_frame();
  reg[1] = top->local_vars[0];
  reg[2] = top->local_vars[1];

  reg[0] = reg[2];
}

static void trap(void)
{
  if(ip_trap_code == TRAP_ERROR) {
    dump_fs();
  }
  ip_trap_code=TRAP_NONE;
}

static object eval(object expr)
{
  struct frame *top;
  xassert(sp == 0);
  reg[0] = expr;
  push_frame(make_eval_frame(object_toplevel));
  while (sp != 0) {
    xassert(sp > 0);
    top = fs_top();
    switch (top->type) {
      if(ip_trap_code != TRAP_NONE) trap();
      case EVAL_FRAME: pop_eval_frame(); break;
      case APPLY_PRIM_FRAME: pop_apply_prim_frame(); break;
      case EVAL_OPERATOR_FRAME: pop_eval_operator_frame(); break;
      case EVAL_OPERANDS_FRAME: pop_eval_operands_frame(); break;
      default: xassert(FALSE);
    }
  }
  return reg[0];
}

// special forms

// <lambda_list> ::= [<required_params>] [:opt <xparams>] [:rest <param>] [:key <xparams>]
// <required_params> ::= <param> <param> ...
// <xparams> ::= <xparam> <xparam> ...
// <xparam> ::= { <param> | (<param> <initial_value> [<supplyp>]) }

static int valid_xparam_p(object o)
{
  o = o->cons.car;
  if (typep(o, Symbol)) return TRUE;
  return typep(o, Cons) && typep(o->cons.car, Symbol)
    && typep(o = o->cons.cdr, Cons)
    && (o->cons.cdr == object_nil
        || (typep(o = o->cons.cdr, Cons)
          && typep(o->cons.car, Symbol) && o->cons.cdr == object_nil));
}

static int parse_params(object *o)
{
  *o = (*o)->cons.cdr;
  if (!valid_xparam_p(*o)) return FALSE;
  *o = (*o)->cons.cdr;
  while (TRUE) {
    if (*o == object_nil) break;
    if (typep((*o)->cons.car, Keyword)) break;
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
    if (type == Keyword) break;
    else if (type == Symbol) params = params->cons.cdr;
    else if (type == Cons) {
      if (lambda_type == Macro && valid_lambda_list_p(Macro, params->cons.car))
        params = params->cons.cdr;
      else return FALSE;
    }
    else return FALSE;
  }
  if (params->cons.car == object_opt)
    if (!parse_params(&params)) return FALSE;
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    if (typep(params->cons.car, Symbol))
      params = params->cons.cdr;
    else return FALSE;
  }
  if (params->cons.car == object_key)
    if (!parse_params(&params)) return FALSE;
  return params == object_nil;
}

SPECIAL(let)
{
  object e, o, p, k, v;
  if (argc <= 1) mark_error("let: too few argument");
  e = gc_new_env(env);
  o = argv->cons.car;
  while (o != object_nil) {
    if (!typep(o, Cons)) mark_error("let: parameter must be list");
    if (!valid_xparam_p(o)) mark_error("let: illegal argument");
    k = o->cons.car;
    v = object_nil;
    if (typep(k, Cons)) {
      v = k->cons.cdr->cons.car;
      k = k->cons.car;
    }
    p = symbol_find(e, k);
    if (p == NULL) p = object_nil;
    xsplay_add(&e->env.binding, k, p);    // important regist k before eval v to define recursion!
    if (v != object_nil) xsplay_replace(&e->env.binding, k, todo_eval(e, v));
    o = o->cons.cdr;
  }
  return eval_sequential(e, argv->cons.cdr);
}

SPECIAL(assign)
{
  object e, s, v;
  if (argc == 0) return object_nil;
  if (argc % 2 != 0) mark_error("<-: must be pair");
  while (argc != 0) {
    e = env;
    s = argv->cons.car;
    argv = argv->cons.cdr;
    v = argv->cons.car;
    argv = argv->cons.cdr;
    if (!typep(s, Symbol)) mark_error("<-: cannot bind except symbol");
    if (s == object_nil) mark_error("<-: cannot bind nil");
    v = todo_eval(e, v);
    while (TRUE) {
      if (e == object_nil) {
        xsplay_replace(&object_toplevel->env.binding, s, v);
        break;
      } else if (xsplay_find(&e->env.binding, s) != NULL) {
        xsplay_replace(&e->env.binding, s, v);
        break;
      } else e = e->env.top;
    }
    argc -= 2;
  }
  return v;
}

SPECIAL(macro)
{
  object sym, params, result;
  if (listp(argv->cons.car)) {
    sym = NULL;
    params = argv->cons.car;
  } else {
    sym = argv->cons.car;
    params = (argv = argv->cons.cdr)->cons.car;
  }
  if (!valid_lambda_list_p(Macro, params))
    mark_error("macro: illegal parameter list");
  result = gc_new_macro(env, params, argv->cons.cdr);
  if (sym != NULL) xsplay_add(&env->env.binding, sym, result);
  return result;
}

SPECIAL(lambda)
{
  object params;
  params = argv->cons.car;
  if (!valid_lambda_list_p(Lambda, params))
    mark_error("lambda: illegal parameter list");
  return gc_new_lambda(env, params, argv->cons.cdr);
}

SPECIAL(quote)
{
  if (argc != 1) {
    if (argc == 0) mark_error("quote: requires argument");
    else mark_error("quote: too many arguments");
  }
  return argv->cons.car;
}

SPECIAL(if)
{
  object test;
  if (argc != 2 && argc != 3) mark_error("if: illegal arguments");
  test = todo_eval(env, argv->cons.car);
  if (test != object_nil) return todo_eval(env, argv->cons.cdr->cons.car);
  if (argc > 2) return todo_eval(env, argv->cons.cdr->cons.cdr->cons.car);
  return object_nil;
}

SPECIAL(begin)
{
  return eval_sequential(env, argv);
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
  sp = 0;
  ip_trap_code = TRAP_NONE;
  for (o = object_boot->lambda.body; o != object_nil; o = o->cons.cdr) {
    p = eval(o->cons.car);
    if (VERBOSE_P) printf("%s\n", object_describe(p, buf));
  }
}
