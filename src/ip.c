// interpreter

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "gc.h"
#include "prim.h"
#include "ip.h"

extern int (*prim_table[])(object args, object *result);
extern char *prim_name_table[];

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

// primitive

// <params> ::= [<params>] [:opt <param_values>] [:rest <param>] [:key <param_values>]
// <params> ::= <param> <param> ...
// <param_values> ::= <param_value> <param_value> ...
// <param_value> ::= { <param> | (<param> <value>) }

static int valid_param_value_p(object o) {
  if (typep(o, Symbol)) return TRUE;
  if (!typep(o, Cons)) return FALSE;
  if (!typep(o->cons.car, Symbol)) return FALSE;
  o = o->cons.cdr;
  return o == object_nil || o->cons.cdr == object_nil;
}

#define param (params->cons.car)

#define next_params() { \
  params = params->cons.cdr; \
  if (!listp(params)) return FALSE; \
}

#define next_param_values() { \
  next_params(); \
  while (params != object_nil) { \
    if (!valid_param_value_p(param)) return FALSE; \
    next_params(); \
    if (typep(param, Keyword)) break; \
  } \
}

static int valid_param_p(object params) {
  if (params == object_nil) return TRUE;
  if (!typep(params, Cons)) return FALSE;
  while (params != object_nil) {
    if (typep(param, Symbol)) {
      next_params();
      continue;
    }
    if (typep(param, Keyword)) break;
    return FALSE;
  }
  if (param == object_opt) next_param_values();
  if (param == object_rest) {
    next_params();
    if (!typep(param, Symbol)) return FALSE;
    next_params();
  }
  if (param == object_key) next_param_values();
  return params == object_nil;
}

#undef param
#undef next_params
#undef next_param_values

static object eval(object e, object o);

static object special_assign(object e, object args)
{
  int i, argc;
  object sym, val;
  ARGC(argc);
  if (argc % 2 != 0) xerror("<-: must be pair");
  for (i = 0; i < argc - 1; i += 2) {
    ARG(i, sym);
    ARG(i + 1, val);
    if (!typep(sym, Symbol)) xerror("<-: cannot bind except symbol");
    bind(e, sym, val = eval(e, val));
  }
  return val;
}

static object special_lambda(object e, object args)
{
  object params;
  ARG(0, params);
  if (!valid_param_p(params)) xerror("lambda: illegal paramter list");
  return gc_new_lambda(e, params, args->cons.cdr, -1);
}

static object special_quote(object e, object args)
{
  int argc;
  object o;
  ARGC(argc);
  if (argc != 1) {
    if (argc == 0) xerror("quote: requires argument");
    else xerror("quote: too many arguments");
  }
  ARG(0, o);
  return o;
}

static object special_if(object e, object args)
{
  int b, argc;
  object test, then, els;
  ARGC(argc);
  ARG(0, test);
  test = eval(e, test);
  if (argc != 2 && argc != 3) xerror("if: illegal arguments");
  switch (type(test)) {
    case Fbarray: b = test->fbarray.size != 0; break;
    case Farray: b = test->farray.size != 0; break;
    case Xint: b = test->xint.val != 0; break;
    case Xfloat: b = test->xfloat.val != 0; break;
    case Symbol: b = test != object_nil && test != object_false; break;
    default: b = TRUE;
  }
  if (b) {
    ARG(1, then);
    return eval(e, then);
  }
  if (argc > 2) {
    ARG(2, els);
    return eval(e, els);
  }
  return object_false;
}

// evaluater

static object eval_operands(object e, object o)
{
  object p;
  if (o == object_nil) return object_nil;
  p = eval(e, o->cons.car);
  return gc_new_cons(p, eval_operands(e, o->cons.cdr));
}

static object eval_sequential(object e, object o)
{
  object p;
  while (o != object_nil) {
    p = eval(e, o->cons.car);
    o = o->cons.cdr;
  }
  return p;
}

static object apply(object operator, object operands)
{
  object e, params, result;
  if (operator->lambda.prim_cd >= 0) {
    if (!(*prim_table[operator->lambda.prim_cd])(operands, &result))
      xerror("primitive '%s' failed"
          , prim_name_table[operator->lambda.prim_cd]);
  } else {
    e = gc_new_env(operator->lambda.env);
    params = operator->lambda.params;
    // required parameter
    while (params != object_nil) {
      if (operands == object_nil) xerror("too few arguments");
      if (params->cons.car == object_opt
          || params->cons.car == object_rest
          || params->cons.car == object_key) break;
      xsplay_add(&e->env.binding, params->cons.car, operands->cons.car);
      params = params->cons.cdr;
      operands = operands->cons.cdr;
    }
    if (operands != object_nil) xerror("too many arguments");
    // TODO ... other parameter
    // evaluate
    result = eval_sequential(e, operator->lambda.body);
  }
  return result;
}

static object eval(object e, object o)
{
  object p, operator, operands;
  switch (type(o)) {
    case Lambda:
    case Fbarray:
    case Farray:
    case Xint:
    case Xfloat:
    case Keyword:
      return o;
    case Symbol:
      if ((p = find(e, o)) == NULL)
        xerror("unbind symbol '%s'", o->symbol.name);
      return p;
    case Cons:
      operator = eval(e, o->cons.car);
      operands = o->cons.cdr;
      if (operator == object_if) return special_if(e, operands);
      if (operator == object_quote) return special_quote(e, operands);
      if (operator == object_assign) return special_assign(e, operands);
      if (operator == object_lambda) return special_lambda(e, operands);
      if (typep(operator, Lambda)) {
        operands = eval_operands(e, o->cons.cdr);
        return apply(operator, operands);
      }
      xerror("not a operator");
    default:
      xerror("illegal object type '%d'", type(o));
      return NULL;
  }
}

void ip_start(object args)
{
#ifdef NDEBUG
  apply(object_toplevel, args, object_nil);
#else
  object o;
  for (o = args->lambda.body; o != object_nil; o = o->cons.cdr)
    object_dump(eval(object_toplevel, o->cons.car));
#endif
}
