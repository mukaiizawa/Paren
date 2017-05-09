/*
  paren primitive.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "sexpr.h"
#include "prim.h"

S *in;
S *out;
S *err;

static S *Function_isNil(S *expr) {
  return NILP(expr)? t: nil;
}

static S *Function_isAtom(S *expr) {
  return ATOMP(expr)? t: nil;
}

static S *Function_car(S *expr) {
  if (S_length(expr) != 1)
    return Error_new("car: Illegal argument exception.");
  if (NILP(FIRST(expr)))
    return nil;
  if (FIRST(expr)->Cons.type != Cons)
    return Error_new("car: not a list.");
  return FIRST(FIRST(expr));
}

static S *Function_cdr(S *expr) {
  if (S_length(expr) != 1)
    return Error_new("cdr: Illegal argument exception.");
  if (NILP(FIRST(expr)))
    return nil;
  if (FIRST(expr)->Cons.type != Cons)
    return Error_new("cdr: not a list.");
  return SECOND(expr);
}

static S *Function_cons(S *expr) {
  if (S_length(expr) != 2)
    return Error_new("cons: Illegal argument exception.");
  if (SECOND(expr)->Cons.type != Cons && !NILP(SECOND(expr)))
    return Error_new("cons: Illegal argument exception.");
  return Cons_new(FIRST(expr), SECOND(expr));
}

static S *Function_list(S *expr) {
  return expr;
}

static S *Function_length(S *expr) {
  int count;
  count = 0;
  if (NILP(expr))
    return Number_new(count);
  else if (expr->Cons.type == Cons)
    return Number_new(S_length(FIRST(expr)));
  else
    return Error_new("length: cannnot apply.");
}

static S *Function_desc(S *expr) {
  printf("address: %d\n", (int)expr);
  printf("type: %s\n", Type_asString(expr->Cons.type));
  switch (expr->Cons.type) {
    case Cons:
      printf("car: %d\n", (int)FIRST(expr));
      printf("cdr: %d\n", (int)REST(expr));
      break;
    case Map:
      break;
    case Symbol:
      printf("name: %s\n", expr->Symbol.val);
      break;
    case Keyword:
      printf("name: %s\n", expr->Keyword.val);
      break;
    case String:
      printf("value: %s\n", expr->String.val);
      break;
    case Character:
      printf("value: %c\n", expr->Character.val);
      break;
    case Number:
      printf("value: %f\n", expr->Number.val);
      break;
    case Function:
      break;
    case Stream:
      break;
    case Error:
      break;
    default:
      break;
  }
  return expr;
}

// static S *Map_put(S *expr) {
//   S *obj, *key, *val;
//   struct MapNode *node;
//   if (S_length(expr) != 3)
//     return Error_new("Map.put: Illegal argument exception.");
//   node = MapNode_alloc();
//   obj = first(expr);
//   key = second(expr);
//   val = third(expr);
//   node->next = obj->Map.head->next;
//   obj->Map.head = node;
//   return obj;
// }

// static S *Map_get(S *expr) {
//   S *obj, *key;
//   struct MapNode *node;
//   if (S_length(expr) != 2)
//     return Error_new("Map.put: Illegal argument exception.");
//   obj = first(expr);
//   key = second(expr);
//   node = obj->Map.head;
//   while ((node = node->next) != NULL) {
//   }
//   return obj;
// }

// S *plus(S *args) {
//   S *sum, *car;
//   sum = S_alloc();
//   sum->Number.type = Number;
//   sum->Number.val = 0;
//   while (!NILP(args)) {
//     car = args->Cons.car;
//     args = args->Cons.cdr;
//     if (car->Cons.type != Number) {
//       return S_new(Error, "+: illegal number.");
//     }
//     sum->Number.val += car->Number.val;
//   }
//   return sum;
// }

// S *asString(S *expr) {
//   S *new;
//   new = S_alloc();
//   new->Cons.type = String;
//   new->string = expr->string;
//   return new;
// }

void Prim_init(S *env) {
  t = Symbol_new("t");
  nil = Symbol_new("nil");
  in = Stream_new(stdin);
  out = Stream_new(stdout);
  err = Stream_new(stderr);
  // Env_install(env, "t", t);
  // Env_install(env, "nil", nil);
  // Map_install(env, "stdin", in);
  // Map_install(env, "stdout", out);
  // Map_install(env, "stderr", err);
  // Map_install(env, "null?", Function_new(Function_isNil, NULL));
  // Map_install(env, "atom?", Function_new(Function_isAtom, NULL));
  // Map_install(env, "car", Function_new(Function_car, NULL));
  // Map_install(env, "cdr", Function_new(Function_cdr, NULL));
  // Map_install(env, "cons", Function_new(Function_cons, NULL));
  // Map_install(env, "list", Function_new(Function_list, NULL));
  // Map_install(env, "length", Function_new(Function_length, NULL));
  // Map_install(env, "desc", Function_new(Function_desc, NULL));
}
