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

// TODO dispatch parameters
PRIM(lambda)
{
  object params, param, body;
  ARG(0, params);
  body = (*args)->cons.cdr;
  while (params != object_nil) {
    if (param->header.type != cons) return FALSE;
    if ((param = params->cons.car)->header.type == symbol) {
    } else if (param->header.type == keyword) {
    } else return FALSE;
    params = params->cons.cdr;
  }
  *result = object_alloc();
  (*result)->header.type = lambda;
  (*result)->lambda.top = params;
  (*result)->lambda.body = body;
  (*result)->lambda.prim_cd = -1;
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

static object eval(object o);
static void print(object o);

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
    while (params->header.type == cons) {
      // parse required parameter
      while (params->cons.car->header.type  == symbol) {
        if (args->header.type != cons) xerror("too few argument");
        if (params->header.type != cons) xerror("too many argument");
        bind(params->cons.car, eval(args->cons.car));
        args = args->cons.cdr;
        params = params->cons.cdr;
      }
      // TODO
    }
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
