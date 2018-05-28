// interpreter

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int sp;
static object call_stack;

static struct xsplay special_splay;
static struct xsplay prim_splay;

static void dump_call_stack(void)
{
  int i;
  printf("\n%s\n", "*** stack trace ***");
  for (i = 0; i < sp; i++) {
    printf("at ");
    object_dump(call_stack->farray.elt[i]);
  }
}

// evaluater
static object eval(object env, object expr);

static object eval_operands(object env, object expr)
{
  object o;
  if (expr == object_nil) return object_nil;
  o = eval(env, expr->cons.car);
  expr = expr->cons.cdr;
  if (!listp(expr)) xerror("parameter must be pure list");
  return gc_new_cons(o, eval_operands(env, expr));
}

static object eval_sequential(object env, object expr)
{
  object o;
  while (expr != object_nil) {
    if (!listp(expr)) xerror("eval_sequential: is not a list");
    o = eval(env, expr->cons.car);
    expr = expr->cons.cdr;
  }
  return o;
}

static void next_operands(object *operands)
{
  *operands = (*operands)->cons.cdr;
  if (!listp(*operands)) xerror("illegal arguments");
}

static void bind_lambda_list(object env, object params, object operands)
{
  int rest_p;
  object k, v;
  rest_p = FALSE;
  while (params != object_nil && !typep(params->cons.car, Keyword)) {
    if (operands == object_nil) xerror("too few arguments");
    if (typep(params->cons.car, Cons))
      bind_lambda_list(env, params->cons.car, operands->cons.car);
    else xsplay_add(&env->env.binding, params->cons.car, operands->cons.car);
    params = params->cons.cdr;
    operands = operands->cons.cdr;
  }
  if (params->cons.car == object_opt) {
    params = params->cons.cdr;
    while (params != object_nil && !typep(params->cons.car, Keyword)) {
      if (!typep(params->cons.car, Cons)) {
        k = params->cons.car;
        v = object_nil;
      } else {
        k = params->cons.car->cons.car;
        v = params->cons.car->cons.cdr->cons.car;
      }
      params = params->cons.cdr;
      if (operands != object_nil) {
        v = operands->cons.car;
        next_operands(&operands);
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
    while (params != object_nil) {
      if (!typep(params->cons.car, Cons)) {
        k = params->cons.car;
        v = object_nil;
      } else {
        k = params->cons.car->cons.car;
        v = params->cons.car->cons.cdr->cons.car;
      }
      xsplay_add(&env->env.binding, k, v);
      params = params->cons.cdr;
    }
    while (operands != object_nil) {
      if (!typep(operands->cons.car, Keyword))
        xerror("illegal keyword parameter");
      k = gc_new_symbol(operands->cons.car->symbol.name + 1);    // skip ':'
      if ((v = xsplay_find(&env->env.binding, k)) == NULL)
        xerror("undefined keyword parameter ':%s'", k->symbol.name);
      next_operands(&operands);
      v = operands->cons.car;
      xsplay_replace(&env->env.binding, k, v);
      next_operands(&operands);
    }
  }
  if (!rest_p && operands != object_nil) xerror("too many arguments");
}

static object apply(object operator, object operands)
{
  object e;
  e = gc_new_env(operator->lambda.env);
  bind_lambda_list(e, operator->lambda.params, operands);
  return eval_sequential(e, operator->lambda.body);
}

static object eval(object env, object expr)
{
  int argc;
  object (*special)(object, int, object);
  object (*prim)(int, object, object *);
  object e, operator, operands, result;
  switch (type(expr)) {
    case Lambda:
    case Fbarray:
    case Farray:
    case Xint:
    case Xfloat:
    case Keyword:
      result = expr;
      break;
    case Symbol:
      e = env;
      while (e != object_nil) {
        if ((result = xsplay_find(&e->env.binding, expr)) != NULL) break;
        e = e->env.top;
      }
      if (result == NULL) xerror("unbind symbol '%s'", expr->symbol.name);
      break;
    case Cons:
      operator = eval(env, expr->cons.car);
      operands = expr->cons.cdr;
      argc = object_length(operands);
      call_stack->farray.elt[sp++] = operator;
      result = NULL;
      switch (type(operator)) {
        case Symbol:
          if ((special = xsplay_find(&special_splay, operator)) != NULL) {
            result = (*special)(env, argc, operands);
          } else if ((prim = xsplay_find(&prim_splay, operator)) != NULL) {
            if (!(*prim)(argc, eval_operands(env, operands), &result))
              xerror("primitive '%s' failed", operator->symbol.name);
          }
          break;
        case Macro:
          result = eval(env, apply(operator, operands));
          break;
        case Lambda:
          result = apply(operator, eval_operands(env, operands));
          break;
        default: break;
      }
      if (result == NULL) xerror("not a operator");
      dump_call_stack();
      sp--;
      break;
    default:
      xerror("eval: illegal object type '%d'", type(expr));
  }
  return result;
}

// special forms

// <params> ::= [<params>] [:opt <param_values>] [:rest <param>] [:key <param_values>]
// <params> ::= <param> <param> ...
// <param_values> ::= <param_value> <param_value> ...
// <param_value> ::= { <param> | (<param> <value>) }

static int valid_param_value_p(object params)
{
  object param;
  if (!typep(params, Cons)) return FALSE;
  param = params->cons.car;
  if (typep(param, Symbol)) return TRUE;
  return typep(param, Cons) && typep(param->cons.car, Symbol)
    && typep((param = param->cons.cdr), Cons) && param->cons.cdr == object_nil;
}

static int fetch_param_value(object *params)
{
  if (!valid_param_value_p(*params)) return FALSE;
  *params = (*params)->cons.cdr;
  return TRUE;
}

static int fetch_param_values(object *params)
{
  *params = (*params)->cons.cdr;
  if (!fetch_param_value(params)) return FALSE;
  while (TRUE) {
    if (*params == object_nil) return TRUE;
    if (!typep((*params), Cons)) return FALSE;
    if (typep((*params)->cons.car, Keyword)) return TRUE;
    if (!fetch_param_value(params)) return FALSE;
  }
}

static int valid_lambda_list_p(int lambda_type, object params)
{
  int type;
  while (TRUE) {
    if (params == object_nil) return TRUE;
    if (!typep(params, Cons)) return FALSE;
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
  if (params->cons.car == object_opt) {
    if (!fetch_param_values(&params)) return FALSE;
  }
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    if (typep(params, Cons) && typep(params->cons.car, Symbol))
      params = params->cons.cdr;
    else return FALSE;
  }
  if (params->cons.car == object_key) {
    if (!fetch_param_values(&params)) return FALSE;
  }
  return params == object_nil;
}

static object eval(object env, object o);

SPECIAL(assign)
{
  object e, s, v;
  if (argc == 0) return object_nil;
  if (argc % 2 != 0) xerror("<-: must be pair");
  while (argc != 0) {
    e = env;
    s = argv->cons.car;
    argv = argv->cons.cdr;
    v = argv->cons.car;
    argv = argv->cons.cdr;
    if (!typep(s, Symbol)) xerror("<-: cannot bind except symbol");
    v = eval(e, v);
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
  if (!typep((sym = argv->cons.car), Symbol))
    xerror("macro: required macro name");
  params = (argv = argv->cons.cdr)->cons.car;
  if (!valid_lambda_list_p(Macro, params))
    xerror("macro: illegal parameter list");
  result = gc_new_macro(env, params, argv->cons.cdr);
  xsplay_add(&env->env.binding, sym, result);
  return result;
}

SPECIAL(lambda)
{
  object params;
  params = argv->cons.car;
  if (!valid_lambda_list_p(Lambda, params))
    xerror("lambda: illegal parameter list");
  return gc_new_lambda(env, params, argv->cons.cdr);
}

SPECIAL(quote)
{
  if (argc != 1) {
    if (argc == 0) xerror("quote: requires argument");
    else xerror("quote: too many arguments");
  }
  return argv->cons.car;
}

static int object_true_p(object o)
{
  switch (type(o)) {
    case Fbarray: return o->fbarray.size != 0;
    case Farray: return o->farray.size != 0;
    case Xint: return o->xint.val != 0;
    case Xfloat: return o->xfloat.val != 0;
    case Symbol: return o != object_nil && o != object_false;
    default: return TRUE;
  }
}

SPECIAL(if)
{
  object test;
  if (argc != 2 && argc != 3) xerror("if: illegal arguments");
  test = eval(env, argv->cons.car);
  if (object_true_p(test)) return eval(env, argv->cons.cdr->cons.car);
  if (argc > 2) return eval(env, argv->cons.cdr->cons.cdr->cons.car);
  return object_false;
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
  object o, p;
  init_builtin();
  sp = 0;
  call_stack = gc_new_farray(DEFAULT_CALL_STACK_SIZE);
  for (o = object_boot->lambda.body; o != object_nil; o = o->cons.cdr) {
    p = eval(object_toplevel, o->cons.car);
    if (VERBOSE_P) object_dump(p);
    gc_chance();
  }
}
