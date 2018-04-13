// interpreter

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "prim.h"
#include "ip.h"

static object toplevel;
static object env;

extern int (*prim_table[])(object *args, object *result);
extern char *prim_name_table[];

static object eval(object o);
static void print(object o);

static object find(object sym)
{
  object o, e;
  e = env;
  while (e != object_nil) {
    if ((o = xsplay_find(&e->lambda.binding, sym)) != NULL) return o;
    e = e->lambda.top;
  }
  return NULL;
}

static void bind(object sym, object val)
{
  object e;
  e = env;
  while (e != object_nil) {
    if (xsplay_find(&e->lambda.binding, sym) != NULL) {
      xsplay_delete(&e->lambda.binding, sym);
      xsplay_add(&e->lambda.binding, sym, val);
      return;
    }
    e = e->lambda.top;
  }
  xsplay_add(&toplevel->lambda.binding, sym, val);
}

// primitive

// int object_bool(object o) {
//   xassert(o != NULL);
//   switch (o->header.type) {
//     case lambda:
//     case cons:
//     case keyword:
//       return TRUE;
//     case fbarray:
//       return o->fbarray.size != 0;
//     case farray:
//       return o->farray.size != 0;
//     case xint:
//       return o->xint.val != 0;
//     case xfloat:
//       return o->xfloat.val != 0.0;
//     case symbol:
//       return object_nilp(o) || o == object_false;
//   }
// }

int symcmp(object o, object p);

// <params> ::= [<params>] [:opt <param_values>] [:rest <param>] [:key <param_values>]
// <params> ::= <param> <param> ...
// <param_values> ::= <param_value> <param_value> ...
// <param_value> ::= { <param> | (<param> <value>) }

static int valid_param_value_p(object o) {
  if (o->header.type == symbol) return TRUE;
  if (o->header.type != cons) return FALSE;
  if (o->cons.car->header.type != symbol) return FALSE;
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
    if (param->header.type == keyword) break; \
  } \
}

static int valid_param_p(object params) {
  if (params == object_nil) return TRUE;
  if (params->header.type != cons) return FALSE;
  while (params != object_nil) {
    if (param->header.type == symbol) {
      next_params();
      continue;
    }
    if (param->header.type == keyword) break;
    return FALSE;
  }
  if (param == object_opt) next_param_values();
  if (param == object_rest) {
    next_params();
    if (param->header.type != symbol) return FALSE;
    next_params();
  }
  if (param == object_key) next_param_values();
  return params == object_nil;
}

#undef next_param_values
#undef next_params
#undef param

PRIM(lambda)
{
  object params, body;
  ARG(0, params);
  if (!valid_param_p(params)) return FALSE;
  body = (*args)->cons.cdr;
  *result = object_alloc();
  (*result)->header.type = lambda;
  (*result)->lambda.top = env;
  (*result)->lambda.params = params;
  (*result)->lambda.body = body;
  (*result)->lambda.prim_cd = -1;
  xsplay_init(&(*result)->lambda.binding, (int(*)(void *, void *))symcmp);
  return TRUE;
}

// PRIM(assign)
// {
//   int i;
//   if (argc % 2 != 0) return FALSE;
//   for (i = 0; i <= argc / 2; i+= 2) {
//     if (!object_typep(arg(i), symbol)) return FALSE;
//     *result = eval(arg(i + 1));
//     bind(arg(i), *result);
//   }
//   return TRUE;
// }
//
// PRIM(quote)
// {
//   if (argc != 1) return FALSE;
//   *result = arg(0);
//   return TRUE;
// }
//
// PRIM(if)
// {
//   if (argc != 2 && argc != 3) return FALSE;
//   if (object_bool(arg(0))) *result = eval(arg(1));
//   else if (argc > 2) *result = eval(arg(2));
//   else *result = object_false;
//   return TRUE;
// }

// evaluater

static object eval_list(object o)
{
  object ope, args, result, params, body;
  ope = eval(o->cons.car);
  args = o->cons.cdr;
  if (ope->header.type != lambda) {
    print(ope);
    xerror("is not a operators");
  }
  if (o->lambda.prim_cd >= 0) {
    if (!(*prim_table[ope->lambda.prim_cd])(&args, &result))
      xerror("primitive '%s' failed", prim_name_table[ope->lambda.prim_cd]);
  } else {
    env = ope;
    params = ope->lambda.params;

    // required parameter
    while (params != object_nil) {
      if (args == object_nil) xerror("too few argument");
      bind(params->cons.car, eval(args->cons.car));
      params = params->cons.cdr;
      args = args->cons.cdr;
    }
    // evaluate
    body = ope->lambda.body;
    while (body != object_nil) {
      result = eval(body->cons.car);
      body = body->cons.cdr;
    }
    env = ope->lambda.top;
  }
  return result;
}

static object eval(object o)
{
  object p;
  switch (o->header.type) {
    case lambda:
    case fbarray:
    case farray:
    case xint:
    case xfloat:
    case keyword:
      return o;
    case symbol:
      if ((p = find(o)) == NULL)
        xerror("unbind symbol '%s'", o->symbol.name);
      return p;
    case cons:
      return eval_list(o);
    default: xerror("illegal object"); return NULL;
  }
}

static void print(object o) {
  object_dump(o);
}

void ip_start(object arg)
{
  object o;
  toplevel = env = arg;
  o = env->lambda.body;
  while (o != object_nil) {
    print(eval(o->cons.car));
    o = o->cons.cdr;
  }
}
