// interpreter

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static struct xsplay special_splay;
static struct xsplay prim_splay;

static object find(object e, object s)
{
  object o;
  while (e != object_nil) {
    if ((o = xsplay_find(&e->env.binding, s)) != NULL) return o;
    e = e->env.top;
  }
  return NULL;
}

static void bind(object e, object s, object v)
{
  while (e != object_nil) {
    if (xsplay_find(&e->env.binding, s) != NULL) {
      xsplay_replace(&e->env.binding, s, v);
      return;
    }
    e = e->env.top;
  }
  xsplay_add(&object_toplevel->env.binding, s, v);
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

static int valid_lambda_list_p(object params)
{
  // required parameter
  while (TRUE) {
    if (params == object_nil) return TRUE;
    if (!typep(params, Cons)) return FALSE;
    if (typep(params->cons.car, Keyword)) break;
    if (!typep(params->cons.car, Symbol)) return FALSE;
    params = params->cons.cdr;
  }
  if (params->cons.car == object_opt)
    if (!fetch_param_values(&params)) return FALSE;
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    if (!(typep(params, Cons) && typep(params->cons.car, Symbol))) return FALSE;
    params = params->cons.cdr;
  }
  if (params->cons.car == object_key)
    if (!fetch_param_values(&params)) return FALSE;
  return params == object_nil;
}

static object eval(object env, object o);

SPECIAL(assign, <-)
{
  object sym, val;
  if (argc % 2 != 0) xerror("<-: must be pair");
  if (argc == 0) return object_nil;
  while (argc != 0) {
    sym = argv->cons.car;
    argv = argv->cons.cdr;
    val = argv->cons.car;
    argv = argv->cons.cdr;
    if (!typep(sym, Symbol)) xerror("<-: cannot bind except symbol");
    bind(env, sym, val = eval(env, val));
    argc -= 2;
  }
  return val;
}

SPECIAL(lambda, lambda)
{
  object params;
  params = argv->cons.car;
  if (!valid_lambda_list_p(params)) xerror("lambda: illegal parameter list");
  return gc_new_lambda(env, params, argv->cons.cdr);
}

SPECIAL(quote, quote)
{
  if (argc != 1) {
    if (argc == 0) xerror("quote: requires argument");
    else xerror("quote: too many arguments");
  }
  return argv->cons.car;
}

SPECIAL(if, if)
{
  int b;
  object test;
  test = eval(env, argv->cons.car);
  if (argc != 2 && argc != 3) xerror("if: illegal arguments");
  switch (type(test)) {
    case Fbarray: b = test->fbarray.size != 0; break;
    case Farray: b = test->farray.size != 0; break;
    case Xint: b = test->xint.val != 0; break;
    case Xfloat: b = test->xfloat.val != 0; break;
    case Symbol: b = test != object_nil && test != object_false; break;
    default: b = TRUE;
  }
  if (b) return eval(env, argv->cons.cdr->cons.car);
  if (argc > 2) return eval(env, argv->cons.cdr->cons.cdr->cons.car);
  return object_false;
}

// evaluater

static object eval_operands(object env, object expr)
{
  object o;
  if (expr == object_nil) return object_nil;
  o = eval(env, expr->cons.car);
  return gc_new_cons(o, eval_operands(env, expr->cons.cdr));
}

static object eval_sequential(object env, object expr)
{
  object o;
  while (expr != object_nil) {
    o = eval(env, expr->cons.car);
    expr = expr->cons.cdr;
  }
  return o;
}

static object apply(object operator, object operands)
{
  object e, k, v, params, result;
  e = gc_new_env(operator->lambda.env);
  params = operator->lambda.params;

  while (params != object_nil && !typep(params->cons.car, Keyword)) {
    if (operands == object_nil) xerror("too few arguments");
    xsplay_add(&e->env.binding, params->cons.car, operands->cons.car);
    params = params->cons.cdr;
    operands = operands->cons.cdr;
  }

  if (params->cons.car == object_opt) {
    params = params->cons.cdr;
    while (params != object_nil && !typep(params->cons.car, Keyword)) {
      if (!typep(params->cons.car, Cons))
        k = params->cons.car;
      else {
        k = params->cons.car->cons.car;
        v = params->cons.car->cons.cdr->cons.car;
      }
      params = params->cons.cdr;
      if (operands != object_nil) {
        v = operands->cons.car;
        operands = operands->cons.cdr;
      }
      xsplay_add(&e->env.binding, k, v);
    }
  }

  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    xsplay_add(&e->env.binding, params->cons.car, operands);
    params = params->cons.cdr;
  }

  if (operands != object_nil) xerror("too many arguments");

  // evaluate
  result = eval_sequential(e, operator->lambda.body);
  return result;
}

static object eval(object env, object expr)
{
  int argc;
  object (*special)(object, int, object);
  object (*prim)(int, object, object *);
  object operator, operands, result;
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
      if ((result = find(env, expr)) == NULL)
        xerror("unbind symbol '%s'", expr->symbol.name);
      break;
    case Cons:
      operator = eval(env, expr->cons.car);
      operands = expr->cons.cdr;
      argc = object_length(operands);
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
        case Lambda:
          result = apply(operator, eval_operands(env, operands));
          break;
        default: break;
      }
      if (result == NULL) xerror("not a operator");
      break;
    default:
      xerror("eval: illegal object type '%d'", type(expr));
  }
  return result;
}

static void init_builtin(void)
{
  int i;
  char *s;
  xsplay_init(&special_splay, (int(*)(void *, void *))symcmp);
  xsplay_init(&prim_splay, (int(*)(void *, void *))symcmp);
  for (i = 0; (s = special_name_table[i]) != NULL; i++)
    xsplay_add(&special_splay, gc_new_symbol(s), special_table[i]);
  for (i = 0; (s = prim_name_table[i]) != NULL; i++)
    xsplay_add(&prim_splay, gc_new_symbol(s), prim_table[i]);
}

void ip_start(void)
{
  object o, p;
  init_builtin();
  for (o = object_boot->lambda.body; o != object_nil; o = o->cons.cdr) {
    p = eval(object_toplevel, o->cons.car);
    if (VERBOSE_P) object_dump(p);
    gc_chance();
  }
}
