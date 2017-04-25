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

static char *nil = "nil";

void Prim_init(Env *env) {
  Env_install(env, "PI", S_newExpr(Number, "3.14159265358979323846"));
  Env_install(env, "+", S_newExpr(Function, "3.1415926"));
}

static S *S_alloc() {
  S *expr;
  if ((expr = (S *)calloc(1, sizeof(S))) == NULL) {
    fprintf(stderr, "S_alloc: Cannot allocate memory.");
    exit(1);
  }
  return expr;
}

static S *toParenBoolean(int i) {
  S *boolean;
  boolean = S_alloc();
  boolean->Atom.type = Symbol;
  boolean->Atom.string = (i)?
    "t":
    "nil";
  return boolean;
}

static int toCBoolean(S *expr) {
  return !(expr->Atom.type == Symbol && strcmp(expr->Atom.string, nil) == 0);
}

static int isAtomC(S *expr) {
  return expr->Atom.type != Cons;
}

static int isNilC(S *expr) {
  return expr->Atom.type == Symbol && strcmp(expr->Atom.string, nil) == 0;
}

S *S_newExpr(Type type, char* str) {
  S *expr;
  expr = S_alloc();
  expr->Atom.type = type;
  expr->Atom.string = str;
  switch (type) {
    case Symbol:
    case Keyword:
      break;
    case Character:
      expr->Atom.character = str[0];
      break;
    case Number:
      expr->Atom.number = atof(str);
      break;
    case Function:
    case Error:
    case Cons:
    default:
      break;
  }
  return expr;
}

S *S_newNil() {
  return S_newExpr(Symbol, nil);
}

S *read() {
  return Lex_parseExpr();
}

S *eval(S *expr, Env *env) {
  S *car, *cdr;
  if (isAtomC(expr))
    return (expr->Atom.type != Symbol)?
      expr:
      Env_lookup(env, expr->Atom.string);
  car = expr->Cons.car;
  cdr = expr->Cons.cdr;
  if (!isAtomC(car))
    car = eval(car, env);
  if (1)
    return plus(expr->Cons.cdr);
  if (car->Atom.type != Function) {
    return S_newExpr(Error, "eval: undefined function.");
  }
  return S_newNil();    // to apply function
}

void print(S *expr) {
  int type;
  if (isAtomC(expr)) {
    if (expr->Atom.type == Number)
      fprintf(stdout, "%f\n", expr->Atom.number);
    else 
      fprintf(stdout, "%s\n", expr->Atom.string);
    return;
  }
  fprintf(stdout, "(");
  print(expr->Cons.car);
  for (expr = expr->Cons.cdr; !isNilC(expr); expr = expr->Cons.cdr) {
    if (!isAtomC(expr->Cons.car))
      fprintf(stdout, " ");
    print(expr->Cons.car);
  }
  fprintf(stdout, "\n");
  fflush(stdout);
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
  car->Cons.prev = cdr->Cons.prev = prev;
  return prev;
}

S *reverse(S *expr) {
  S *root;
  root = S_newNil();
  while (!isNilC(expr)) {
    root = cons(expr->Cons.car, root);
    expr = expr->Cons.cdr;
  }
  return root;
}

S *plus(S *args) {
  S *sum, *car, *cdr;
  sum = S_newExpr(Number, "0");
  cdr = args;
  while (!isNilC(args)) {
    car = args->Cons.car;
    cdr = args->Cons.cdr;
    args = args->Cons.cdr;
    if (car->Atom.type != Number) {
      return S_newNil();    // TODO: create error.
    }
    sum->Atom.number += car->Atom.number;
  }
  return sum;
}

S *asString(S *expr) {
  S *new;
  new = S_alloc();
  new->Atom.type = String;
  new->Atom.string = expr->Atom.string;
  return new;
}
