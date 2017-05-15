/*
  paren primitive.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "splay.h"
#include "prim.h"

S *t;
S *nil;
S *in;
S *out;
S *err;

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

static char *TYPE_STRING[10] = {
  "Cons",
  "Map",
  "Symbol",
  "Keyword",
  "String",
  "Character",
  "Number",
  "Function",
  "Stream",
  "Error"
};

char *Type_asString(Type type) {
  return TYPE_STRING[type];
};

static S *S_alloc() {
  S *expr;
  if ((expr = (S *)calloc(1, sizeof(S))) == NULL) {
    fprintf(stderr, "S_alloc: Cannot allocate memory.");
    exit(1);
  }
  return expr;
}

S *Cons_new(S *car, S *cdr) {
  S *prev;
  if (ATOMP(cdr) && !NILP(cdr)) {
    fprintf(stderr, "Cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  prev = S_alloc();
  prev->Cons.type = Cons;
  FIRST(prev) = car;
  REST(prev) = cdr;
  return prev;
}

static struct MapNode *MapNode_alloc() {
  struct MapNode *mapNode;
  if ((mapNode = (struct MapNode *)calloc(1, sizeof(struct MapNode))) == NULL) {
    fprintf(stderr, "MapNode_alloc: Cannot allocate memory.");
    exit(1);
  }
  return mapNode;
}

S *Map_new(S *car, S *cdr) {
  S *expr;
  expr = S_alloc();
  expr->Map.type = Map;
  expr->Map.head = MapNode_alloc();
  return expr;
}

S *Symbol_new(char *val) {
  S *expr;
  expr = S_alloc();
  expr->Symbol.type = Symbol;
  expr->Symbol.val = val;
  return expr;
}

S *Keyword_new(char *val) {
  S *expr;
  expr = S_alloc();
  expr->Keyword.type = Keyword;
  expr->Keyword.val = val;
  return expr;
}

S *String_new(char *val) {
  S *expr;
  expr = S_alloc();
  expr->String.type = String;
  expr->String.val = val;
  return expr;
}

S *Character_new(char val) {
  S *expr;
  expr = S_alloc();
  expr->Character.type = Character;
  expr->Character.val = val;
  return expr;
}

S *Number_new(double val) {
  S *expr;
  expr = S_alloc();
  expr->Number.type = Number;
  expr->Number.val = val;
  return expr;
}

S *Function_new(S *f(S *), S *args) {
  S *expr;
  expr = S_alloc();
  expr->Function.type = Function;
  expr->Function.f = f;
  expr->Function.args = args;
  return expr;
}

S *Stream_new(FILE *stream) {
  S *expr;
  expr = S_alloc();
  expr->Stream.type = Stream;
  expr->Stream.stream = stream;
  return expr;
}

S *Error_new(char *str) {
  S *expr;
  expr = S_alloc();
  expr->Error.type = Error;
  expr->Error.val = str;
  return expr;
}

int S_length(S *expr) {
  int count;
  count = 1;
  if (expr->Cons.type != Cons)
    return count;
  else {
    while (!NILP((expr = REST(expr))))    // safe?
      count++;
    return count;
  }
}

S *S_reverse(S *expr) {
  S *root;
  if (NILP(expr))
    return nil;
  assert(expr->Cons.type == Cons);
  root = nil;
  while (!NILP(expr)) {
    root = Cons_new(FIRST(expr), root);
    expr = REST(expr);
  }
  return root;
}

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
