/*
  paren main routine.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "lex.h"
#include "env.h"
#include "prim.h"

static struct Env *env;

static void init() {
  env = Env_new();
  Env_init(env);
  Prim_init(env);
  Lex_init();
}

S *Paren_read() {
  return Lex_parseExpr();
}

S *Paren_eval(S *expr, Env *env) {
  S *car, *cdr;
  if (isAtomC(expr))
    return (expr->Cons.type == Symbol)?
      Env_lookup(env, expr->Symbol.val):
      expr;
  car = Paren_eval(expr->Cons.car, env);
  cdr = expr->Cons.cdr;
  if (car->Cons.type != Function) {
    return Error_new("Paren_eval: undefined function.");
  }
  return nil; // TODO: to apply function.
}

S *Paren_print(S *expr) {
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
    Paren_print(expr->Cons.car);
    for (expr = expr->Cons.cdr; !isNilC(expr); expr = expr->Cons.cdr) {
      fprintf(stdout, isAtomC(expr->Cons.car)? " ": "");
      Paren_print(expr->Cons.car);
    }
    fprintf(stdout, ")");
  }
  fflush(stdout);
  return expr;
}

int main(int argc, char* argv[]) {
  init();
  while (1) {
    fprintf(stdout, ") ");
    Paren_print(Paren_eval(Paren_read(), env));
    fprintf(stdout, "\n");
  }
  return 0;
}
