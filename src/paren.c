/*
  paren main routine.
*/

#include <stdio.h>
#include <math.h>
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
  S *root, *cmd, *args;
  if (S_isAtom(expr))
    return (expr->Symbol.type == Symbol)?
      Env_lookup(env, expr->Symbol.val):
      expr;
  root = expr;
  while (!S_isNil(expr)) {
    expr->Cons.car = Paren_eval(expr->Cons.car, env);
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
  // invoke user define function.
  else {
    return nil;    // TODO: apply user defined function.
  }
}

S *Paren_print(S *expr) {
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
    Paren_print(expr->Cons.car);
    for (expr = expr->Cons.cdr; !S_isNil(expr); expr = expr->Cons.cdr) {
      fprintf(stdout, S_isAtom(expr->Cons.car)? " ": "");
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
