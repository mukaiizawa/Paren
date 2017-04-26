/*
  paren primitive.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "macro.h"
#include "prim.h"
#include "lex.h"

S *nil;
S *t;

void Prim_init(Env *env) {
  nil = S_new(Symbol, "nil");
  t = S_new(Symbol, "t");
  Env_install(env, "nil", nil);
  Env_install(env, "t", t);
  Env_install(env, "PI", S_new(Symbol, "3.1415926"));
}

static S *S_alloc() {
  S *expr;
  if ((expr = (S *)calloc(1, sizeof(S))) == NULL) {
    fprintf(stderr, "S_alloc: Cannot allocate memory.");
    exit(1);
  }
  return expr;
}

static S *toParenBoolean(int b) {
  return b? t: nil;
}

static int toCBoolean(S *expr) {
  return expr != nil;
}

static int isAtomC(S *expr) {
  return expr->Cons.type != Cons;
}

static int isNilC(S *expr) {
  return expr == nil;
}

S *S_new(Type type, char* str) {
  S *expr;
  expr = S_alloc();
  expr->Cons.type = type;
  switch (type) {
    case Symbol:
      expr->Symbol.val = str;
      break;
    case Keyword:
      expr->Keyword.val = str;
      break;
    case String:
      expr->String.val = str;
      break;
    case Character:
      expr->Character.val = str[0];
      break;
    case Number:
      expr->Number.val = atof(str);
      free(str);
      break;
    case Function:
      break;
    case Cons:
      break;
    case Error:
      expr->Error.val = str;
      break;
    default:
      break;
  }
  return expr;
}

S *read() {
  return Lex_parseExpr();
}

S *eval(S *expr, Env *env) {
  S *car, *cdr;
  if (isAtomC(expr))
    return (expr->Cons.type == Symbol)?
      Env_lookup(env, expr->Symbol.val):
      expr;
  car = eval(expr->Cons.car, env);
  cdr = expr->Cons.cdr;
  return plus(cdr);
  if (car->Cons.type != Function) {
    return S_new(Error, "eval: undefined function.");
  }
  return nil; // TODO: to apply function.
}

S *print(S *expr) {
  int type;
  if (isAtomC(expr)) {
    if (expr->Cons.type == Number)
      fprintf(stdout, "%f", expr->Number.val);
    else if (expr->Cons.type == Character)
      fprintf(stdout, "%c", expr->Character.val);
    else
      fprintf(stdout, "%s", expr->String.val);
  }
  else {
    fprintf(stdout, "(");
    print(expr->Cons.car);
    for (expr = expr->Cons.cdr; !isNilC(expr); expr = expr->Cons.cdr) {
      fprintf(stdout, isAtomC(expr->Cons.car)? " ": "");
      print(expr->Cons.car);
    }
    fprintf(stdout, ")");
  }
  fflush(stdout);
  return expr;
}

S *isAtom(S *expr) {
  return toParenBoolean(isAtomC(expr));
}

S *isNil(S *expr) {
  return toParenBoolean(isNilC(expr));
}

S *cons(S *car, S *cdr) {
  S *prev;
  if (isAtomC(cdr) && !isNilC(cdr)) {
    fprintf(stderr, "Cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  prev = S_alloc();
  prev->Cons.type = Cons;
  prev->Cons.car = car;
  prev->Cons.cdr = cdr;
  return prev;
}

S *reverse(S *expr) {
  S *root;
  root = nil;
  while (!isNilC(expr)) {
    root = cons(expr->Cons.car, root);
    expr = expr->Cons.cdr;
  }
  return root;
}

S *plus(S *args) {
  S *sum, *car;
  sum = S_alloc();
  sum->Number.type = Number;
  while (!isNilC(args)) {
    car = args->Cons.car;
    args = args->Cons.cdr;
    if (car->Cons.type != Number) {
      return S_new(Error, "+: illegal number.");
    }
    sum->Number.val += car->Number.val;
  }
  return sum;
}

// S *asString(S *expr) {
//   S *new;
//   new = S_alloc();
//   new->Cons.type = String;
//   new->string = expr->string;
//   return new;
// }
