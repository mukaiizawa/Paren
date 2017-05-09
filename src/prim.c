/*
  paren primitive.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "macro.h"
#include "prim.h"
#include "lex.h"

S *t;
S *nil;
S *in;
S *out;
S *err;

char *TypeString[10] = {
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

static S *S_alloc() {
  S *expr;
  if ((expr = (S *)calloc(1, sizeof(S))) == NULL) {
    fprintf(stderr, "S_alloc: Cannot allocate memory.");
    exit(1);
  }
  return expr;
}

static struct MapNode *MapNode_alloc() {
  struct MapNode *mapNode;
  if ((mapNode = (struct MapNode *)calloc(1, sizeof(struct MapNode))) == NULL) {
    fprintf(stderr, "S_alloc: Cannot allocate memory.");
    exit(1);
  }
  return mapNode;
}

static S *Function_isNil(S *expr) {
  return S_isNil(expr)? t: nil;
}

static S *Function_isAtom(S *expr) {
  return S_isAtom(expr)? t: nil;
}

static S *Function_car(S *expr) {
  if (S_length(expr) != 1)
    return Error_new("car: Illegal argument exception.");
  if (S_isNil(FIRST(expr)))
    return nil;
  if (FIRST(expr)->Cons.type != Cons)
    return Error_new("car: not a list.");
  return FIRST(FIRST(expr));
}

static S *Function_cdr(S *expr) {
  if (S_length(expr) != 1)
    return Error_new("cdr: Illegal argument exception.");
  if (S_isNil(FIRST(expr)))
    return nil;
  if (FIRST(expr)->Cons.type != Cons)
    return Error_new("cdr: not a list.");
  return SECOND(expr);
}

static S *Function_cons(S *expr) {
  S *prev;
  if (S_length(expr) != 2)
    return Error_new("cons: Illegal argument exception.");
  if (SECOND(expr)->Cons.type != Cons && !S_isNil(SECOND(expr)))
    return Error_new("cons: Illegal argument exception.");
  prev = S_alloc();
  prev->Cons.type = Cons;
  FIRST(prev) = FIRST(expr);
  REST(prev) = SECOND(expr);
  return prev;
}

static S *Function_list(S *expr) {
  return expr;
}

static S *Function_length(S *expr) {
  int count;
  count = 0;
  if (S_isNil(expr))
    return Number_new(count);
  else if (expr->Cons.type == Cons)
    return Number_new(S_length(FIRST(expr)));
  else
    return Error_new("length: cannnot apply.");
}

static S *Function_desc(S *expr) {
  printf("address: %d\n", (int)expr);
  printf("type: %s\n", TypeString[expr->Cons.type]);
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

S *S_read() {
  return Lex_parseExpr();
}

S *S_eval(S *expr, Env *env) {
  S *root, *cmd, *args;
  if (S_isAtom(expr))
    return (expr->Symbol.type == Symbol)?
      Env_lookup(env, expr->Symbol.val):
      expr;
  root = expr;
  while (!S_isNil(expr)) {
    expr->Cons.car = S_eval(expr->Cons.car, env);
    expr = expr->Cons.cdr;
  }
  cmd = root->Cons.car;
  args = root->Cons.cdr;
  if (cmd->Function.type != Function) {
    return Error_new("eval: undefined function.");
  }
  // invoke primitive function.
  if (cmd->Function.f != NULL) {
    return (cmd->Function.f)(args);
  }
  // invoke user defined function.
  else {
    return nil;    // TODO: apply user defined function.
  }
}

S *S_print(S *expr) {
  int type;
  if (S_isAtom(expr)) {
    if (expr->Cons.type == Number) {
      double intptr, fraction;
      fraction = modf(expr->Number.val, &intptr);
      if (fraction == 0)
        fprintf(stdout, "%d", (int)intptr);
      else
        fprintf(stdout, "%f", expr->Number.val);
    }
    else if (expr->Cons.type == Character)
      fprintf(stdout, "%c", expr->Character.val);
    else if (expr->Cons.type == Function)
      fprintf(stdout, "%d", expr->Cons.type);
    else
      fprintf(stdout, "%s", expr->String.val);
  }
  else {
    fprintf(stdout, "(");
    S_print(expr->Cons.car);
    for (expr = expr->Cons.cdr; !S_isNil(expr); expr = expr->Cons.cdr) {
      fprintf(stdout, S_isAtom(expr->Cons.car)? " ": "");
      S_print(expr->Cons.car);
    }
    fprintf(stdout, ")");
  }
  fflush(stdout);
  return expr;
}

int S_length(S *expr) {
  int count;
  count = 1;
  if (expr->Cons.type != Cons)
    return count;
  else {
    while (!S_isNil(expr = REST(expr)))
      count++;
    return count;
  }
}

S *S_reverse(S *expr) {
  S *root;
  if (S_isNil(expr))
    return nil;
  assert(expr->Cons.type == Cons);
  root = nil;
  while (!S_isNil(expr)) {
    root = Cons_new(FIRST(expr), root);
    expr = REST(expr);
  }
  return root;
}

int S_isAtom(S *expr) {
  return expr->Cons.type != Cons;
}

int S_isNil(S *expr) {
  return expr == nil;
}

S *Cons_new(S *car, S *cdr) {
  S *prev;
  if (S_isAtom(cdr) && !S_isNil(cdr)) {
    fprintf(stderr, "Cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  prev = S_alloc();
  prev->Cons.type = Cons;
  FIRST(prev) = car;
  REST(prev) = cdr;
  return prev;
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

S *Error_new(char *str) {
  S *expr;
  expr = S_alloc();
  expr->Error.type = Error;
  expr->Error.val = str;
  return expr;
}

// S *plus(S *args) {
//   S *sum, *car;
//   sum = S_alloc();
//   sum->Number.type = Number;
//   sum->Number.val = 0;
//   while (!S_isNil(args)) {
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

void Prim_init(Env *env) {
  t = Symbol_new("t");
  nil = Symbol_new("nil");
  in = Stream_new(stdin);
  out = Stream_new(stdout);
  err = Stream_new(stderr);
  Env_install(env, "t", t);
  Env_install(env, "nil", nil);
  Env_install(env, "stdin", in);
  Env_install(env, "stdout", out);
  Env_install(env, "stderr", err);
  Env_install(env, "null?", Function_new(Function_isNil, NULL));
  Env_install(env, "atom?", Function_new(Function_isAtom, NULL));
  Env_install(env, "car", Function_new(Function_car, NULL));
  Env_install(env, "cdr", Function_new(Function_cdr, NULL));
  Env_install(env, "cons", Function_new(Function_cons, NULL));
  Env_install(env, "list", Function_new(Function_list, NULL));
  Env_install(env, "length", Function_new(Function_length, NULL));
  Env_install(env, "desc", Function_new(Function_desc, NULL));
}
