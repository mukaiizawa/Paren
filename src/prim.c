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
  boolean->Atom->type = Symbol;
  boolean->Atom->string = (i)?
    "t":
    "nil";
  return boolean;
}

static int toCBoolean(S *expr) {
  return !(expr->Atom->type == Symbol && strcmp(expr->Atom->string, nil) == 0);
}

S *S_newNil() {
  S *expr;
  expr = S_alloc();
  expr->Atom->type = Symbol;
  expr->Atom->string = nil;
  return expr;
}

S *S_newSymbol(char *str) {
  S *expr;
  expr = S_alloc();
  expr->Atom->type = Symbol;
  expr->Atom->string = str;
  return expr;
}

S *S_newKeyword(char *str) {
  S *expr;
  expr = S_alloc();
  expr->Atom->type = Keyword;
  expr->Atom->string = str;
  return expr;
}

S *S_newString(char *str) {
  S *expr;
  expr = S_alloc();
  expr->Atom->type = String;
  expr->Atom->string = str;
  return expr;
}

S *S_newCharacter(char *str) {
  S *expr;
  expr = S_alloc();
  expr->Atom->type = Character;
  expr->Atom->string = str;
  expr->Atom->character = str[0];
  return expr;
}

S *S_newNumber(char *str) {
  S *expr;
  expr = S_alloc();
  expr->Atom->type = Number;
  expr->Atom->string = str;
  expr->Atom->string = str;
  return expr;
}

S *read() {
  return Lex_parseS();
}

S *eval(S *expr, Env *env) {
  S *car, *cdr;
  if (isAtom(expr))
    return expr;
  car = expr->Cons->car;
  cdr = expr->Cons->cdr;
  if (!isAtom(car))
    car = eval(car, env);
  // if (car->Atom->type != Function) {
  //   fprintf(stderr, "eval: '%s' is not a function.\n", asString(first->obj)->val.string);
  //   return S_newNil();
  // }
  // Env_lookup();
  return NULL;
}

void print(S *expr) {
  int type;
  if (isAtom(expr)) {
    fprintf(stdout, "%s\n", expr->Atom->string);
    return;
  }
  fprintf(stdout, "(");
  print(expr->Cons->car);
  for (expr = expr->Cons->cdr; !toCBoolean(isNil(expr)); expr = expr->Cons->cdr) {
    if (!isAtom(expr->Cons->car))
      fprintf(stdout, " ");
    print(expr->Cons->car);
  }
  fprintf(stdout, "\n");
  fflush(stdout);
}

S *isAtom(S *expr) {
  return toParenBoolean(expr->Atom->type != Cons);
}

S *isNil(S *expr) {
  return toParenBoolean(
      (expr->Atom->type == Symbol && strcmp(expr->Atom->string, nil) == 0));
}

S *cons(S *car, S *cdr) {
  S *prev;
  if (isAtom(cdr) && !isNil(cdr)) {
    fprintf(stderr, "Cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  prev = S_alloc();
  prev->Cons->car = car;
  prev->Cons->cdr = cdr;
  car->Cons->prev = cdr->Cons->prev = prev;
  return prev;
}

S *reverse(S *expr) {
  S *root;
  root = S_newNil();
  while (!toCBoolean(isNil(expr))) {
    root = cons(expr->Cons->car, root);
    expr = expr->Cons->cdr;
  }
  return root;
}

S *asString(S *s) {
  S *new;
  new = S_alloc();
  new->Atom->type = String;
  new->Atom->string = s->Atom->string;
  return new;
}
