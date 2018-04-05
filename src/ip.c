// interpreter

#include "std.h"
#include "xsplay.h"
#include "xarray.h"
#include "object.h"
#include "ip.h"

// static object find(object sym)
// {
//   object o, e;
//   xassert(object_typep(sym, symbol));
//   e = env;
//   while (e != object_nil) {
//     if ((o = xsplay_find(&e->lambda.binding, sym)) != NULL) return o;
//     e = e->lambda.top;
//   }
//   return NULL;
// }
//
// static void bind(object sym, object val)
// {
//   object e;
//   xassert(object_typep(sym, symbol));
//   e = env;
//   while (e != object_nil) {
//     xassert(object_typep(e, lambda));
//     if (xsplay_find(&e->lambda.binding, sym) != NULL) {
//       xsplay_delete(&e->lambda.binding, sym);
//       xsplay_add(&e->lambda.binding, sym, val);
//       return;
//     }
//     e = e->lambda.top;
//   }
//   xsplay_add(&toplevel->lambda.binding, sym, val);
// }
//
// static object eval_list(object o)
// {
//   object ope, args, result, params, body;
//   ope = eval(object_car(o));
//   args = object_cdr(o);
//   if (!object_typep(ope, lambda)) {
//     object_dump(ope);
//     lex_error("is not a operators");
//   }
//   if (o->lambda.prim_cd >= 0) {
//     if (!(*prim_table[ope->lambda.prim_cd])(args, &result))
//       lex_error("primitive '%s' failed", prim_name_table[ope->lambda.prim_cd]);
//   } else {
//     env = ope;
//     params = ope->lambda.params;
//     while (object_consp(params)) {
//       // parse required parameter
//       while (object_typep(object_car(params), symbol)) {
//         if (!object_consp(args)) lex_error("too few argument");
//         if (!object_consp(params)) lex_error("too many argument");
//         bind(object_car(params), eval(object_car(args)));
//         args = object_cdr(args);
//         params = object_cdr(params);
//       }
//       // TODO
//     }
//     body = ope->lambda.body;
//     while (!object_nilp(body)) {
//       result = eval(object_car(body));
//       body = object_cdr(body);
//     }
//     env = ope->lambda.top;
//   }
//   return result;
// }
//
// static object eval(object o)
// {
//   object p;
//   switch (o->header.type) {
//     case lambda:
//     case fbarray:
//     case farray:
//     case xint:
//     case xfloat:
//     case keyword:
//       return o;
//     case symbol:
//       if ((p = find(o)) == NULL)
//         lex_error("unbind symbol '%s'", o->symbol.name);
//       return p;
//     case cons:
//       return eval_list(o);
//   }
// }

static void repl()
{
  return;
}

void ip_start(object args[])
{
  repl();
}
