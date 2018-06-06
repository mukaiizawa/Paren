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

void ip_mark(void)
{
  gc_mark(call_stack);
}

static void dump_call_stack(void);

static void ip_error(char *fmt, ...)
{
  char buf[MAX_STR_LEN];
  va_list va;
  va_start(va, fmt);
  xvsprintf(buf, fmt, va);
  va_end(va);
  dump_call_stack();
  xerror(buf);
}

// call stack

static void dump_call_stack(void)
{
  int i;
  char buf[MAX_STR_LEN];
  printf("\n%s\n", "*** stack trace ***");
  for (i = sp; i > 0; i -= 2) {
    printf("at %s\n", object_describe(gc_new_cons(
            call_stack->farray.elt[i - 2], call_stack->farray.elt[i - 1])
          , buf));
  }
}

static void push_call_stack(object o, object p)
{
  if (sp == CALL_STACK_SIZE) {
    dump_call_stack();
    ip_error("stack over flow");
  }
  call_stack->farray.elt[sp++] = o;
  call_stack->farray.elt[sp++] = p;
}

static void pop_call_stack(void)
{
  xassert(sp >= 0);
  sp -= 2;
}

// evaluater
static object eval(object env, object expr);

static object eval_operands(object env, object expr)
{
  object o;
  if (expr == object_nil) return object_nil;
  o = eval(env, expr->cons.car);
  expr = expr->cons.cdr;
  if (!listp(expr)) ip_error("parameter must be pure list");
  return gc_new_cons(o, eval_operands(env, expr));
}

static object eval_sequential(object env, object expr)
{
  object o;
  while (expr != object_nil) {
    if (!listp(expr)) ip_error("eval_sequential: is not a list");
    o = eval(env, expr->cons.car);
    expr = expr->cons.cdr;
  }
  return o;
}

static void next_operands(object *operands)
{
  *operands = (*operands)->cons.cdr;
  if (!listp(*operands)) ip_error("illegal arguments");
}

static void bind_lambda_list(object env, object params, object operands)
{
  int rest_p;
  object o, k, v;
  struct xsplay s;
  rest_p = FALSE;
  while (params != object_nil && !typep(params->cons.car, Keyword)) {
    if (operands == object_nil) ip_error("too few arguments");
    if (typep(params->cons.car, Cons))
      bind_lambda_list(env, params->cons.car, operands->cons.car);
    else xsplay_add(&env->env.binding, params->cons.car, operands->cons.car);
    params = params->cons.cdr;
    operands = operands->cons.cdr;
  }
  if (params->cons.car == object_opt) {
    params = params->cons.cdr;
    while (params != object_nil && !typep(params->cons.car, Keyword)) {
      o = params->cons.car;
      if (!typep(o, Cons)) {
        k = o;
        v = object_nil;
      } else {
        k = o->cons.car;
        v = o->cons.cdr->cons.car;
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
    xsplay_init(&s, (int(*)(void *, void *))symcmp);
    while (params != object_nil) {
      o = params->cons.car;
      if (!typep(o, Cons)) {
        k = o;
        v = object_nil;
      } else {
        k = o->cons.car;
        v = (o = o->cons.cdr)->cons.car;
        if ((o = o->cons.cdr) != object_nil) {
          xsplay_add(&s, k, o->cons.car);
          xsplay_add(&env->env.binding, o->cons.car, object_nil);
        }
      }
      xsplay_add(&env->env.binding, k, v);
      params = params->cons.cdr;
    }
    while (operands != object_nil) {
      if (!typep(operands->cons.car, Keyword))
        ip_error("illegal keyword parameter");
      k = gc_new_symbol(operands->cons.car->symbol.name + 1);    // skip ':'
      if ((v = xsplay_find(&env->env.binding, k)) == NULL)
        ip_error("undefined keyword parameter ':%s'", k->symbol.name);
      next_operands(&operands);
      v = operands->cons.car;
      xsplay_replace(&env->env.binding, k, v);
      if ((o = xsplay_find(&s, k)) != NULL)
        xsplay_replace(&env->env.binding, o, object_true);
      next_operands(&operands);
    }
  }
  if (!rest_p && operands != object_nil) ip_error("too many arguments");
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
    case Macro:
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
      if (result == NULL) ip_error("unbind symbol '%s'", expr->symbol.name);
      break;
    case Cons:
      operator = eval(env, expr->cons.car);
      operands = expr->cons.cdr;
      push_call_stack(operator, operands);
      argc = object_length(operands);
      result = NULL;
      switch (type(operator)) {
        case Symbol:
          if ((special = xsplay_find(&special_splay, operator)) != NULL) {
            result = (*special)(env, argc, operands);
          } else if ((prim = xsplay_find(&prim_splay, operator)) != NULL) {
            if (!(*prim)(argc, eval_operands(env, operands), &result))
              ip_error("primitive '%s' failed", operator->symbol.name);
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
      if (result == NULL) ip_error("not a operator");
      pop_call_stack();
      break;
    default:
      xerror("eval: illegal object type '%d'", type(expr));
  }
  return result;
}

// special forms

// <lambda_list> ::= [<required_params>] [:opt <opt_params>] [:rest <param>] [:key <key_params>]
// <required_params> ::= <param> <param> ...
// <opt_params> ::= <opt> <opt> ...
// <opt> ::= { <param> | (<param> <initial_value>) }
// <key_params> ::= <key> <key> ...
// <key> ::= { <param> | (<param> <initial_value> [<givedp>]) }

static int valid_opt_p(object o)
{
  o = o->cons.car;
  if (typep(o, Symbol)) return TRUE;
  return typep(o, Cons) && typep(o->cons.car, Symbol)
    && typep(o = o->cons.cdr, Cons) && o->cons.cdr == object_nil;
}

static int valid_key_p(object o)
{
  o = o->cons.car;
  if (typep(o, Symbol)) return TRUE;
  return typep(o, Cons) && typep(o->cons.car, Symbol)
    && typep(o = o->cons.cdr, Cons)
    && (o->cons.cdr == object_nil
        || (typep(o = o->cons.cdr, Cons)
          && typep(o->cons.car, Symbol) && o->cons.cdr == object_nil));
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
    params = params->cons.cdr;
    if (!valid_opt_p(params)) return FALSE;
    params = params->cons.cdr;
    while (TRUE) {
      if (params == object_nil) break;
      if (!typep(params, Cons)) return FALSE;
      if (typep(params->cons.car, Keyword)) break;
      if (!valid_opt_p(params)) return FALSE;
      params = params->cons.cdr;
    }
  }
  if (params->cons.car == object_rest) {
    params = params->cons.cdr;
    if (typep(params, Cons) && typep(params->cons.car, Symbol))
      params = params->cons.cdr;
    else return FALSE;
  }
  if (params->cons.car == object_key) {
    params = params->cons.cdr;
    if (!valid_key_p(params)) return FALSE;
    params = params->cons.cdr;
    while (TRUE) {
      if (params == object_nil) break;
      if (!typep(params, Cons)) return FALSE;
      if (typep(params->cons.car, Keyword)) break;
      if (!valid_key_p(params)) return FALSE;
      params = params->cons.cdr;
    }
  }
  return params == object_nil;
}

static object eval(object env, object o);

SPECIAL(let)
{
  object e, o, k, v;
  if (argc <= 1) ip_error("let; too few argument");
  e = gc_new_env(env);
  o = argv->cons.car;
  while (o != object_nil) {
    if (!typep(o, Cons)) ip_error("let: parameter must be list");
    if (!valid_opt_p(o)) ip_error("let: illegal argument");
    k = o->cons.car;
    v = object_nil;
    if (typep(k, Cons)) {
      v = k->cons.cdr->cons.car;
      k = k->cons.car;
    }
    xsplay_add(&e->env.binding, k, object_nil);    // important regist k before eval v to define recursion!
    if (v != object_nil) xsplay_replace(&e->env.binding, k, eval(e, v));
    o = o->cons.cdr;
  }
  return eval_sequential(e, argv->cons.cdr);
}

SPECIAL(assign)
{
  object e, s, v;
  if (argc == 0) return object_nil;
  if (argc % 2 != 0) ip_error("<-: must be pair");
  while (argc != 0) {
    e = env;
    s = argv->cons.car;
    argv = argv->cons.cdr;
    v = argv->cons.car;
    argv = argv->cons.cdr;
    if (!typep(s, Symbol)) ip_error("<-: cannot bind except symbol");
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
  if (typep((sym = argv->cons.car), Symbol))
    params = (argv = argv->cons.cdr)->cons.car;
  else {
    params = sym;
    sym = NULL;
  }
  if (!valid_lambda_list_p(Macro, params))
    ip_error("macro: illegal parameter list");
  result = gc_new_macro(env, params, argv->cons.cdr);
  if (sym != NULL) xsplay_add(&env->env.binding, sym, result);
  return result;
}

SPECIAL(lambda)
{
  object params;
  params = argv->cons.car;
  if (!valid_lambda_list_p(Lambda, params))
    ip_error("lambda: illegal parameter list");
  return gc_new_lambda(env, params, argv->cons.cdr);
}

SPECIAL(quote)
{
  if (argc != 1) {
    if (argc == 0) ip_error("quote: requires argument");
    else ip_error("quote: too many arguments");
  }
  return argv->cons.car;
}

static int object_true_p(object o)
{
  switch (type(o)) {
    case Fbarray: return o->fbarray.size != 0;
    case Farray: return o->farray.size != 0;
    case Symbol: return o != object_nil;
    default: return TRUE;
  }
}

SPECIAL(if)
{
  object test;
  if (argc != 2 && argc != 3) ip_error("if: illegal arguments");
  test = eval(env, argv->cons.car);
  if (object_true_p(test)) return eval(env, argv->cons.cdr->cons.car);
  if (argc > 2) return eval(env, argv->cons.cdr->cons.cdr->cons.car);
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
  call_stack = gc_new_farray(CALL_STACK_SIZE);
  for (o = object_boot->lambda.body; o != object_nil; o = o->cons.cdr) {
    p = eval(object_toplevel, o->cons.car);
    if (VERBOSE_P) printf("%s\n", object_describe(p, buf));
    gc_chance();
  }
}
