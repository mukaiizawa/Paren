/*
  paren main routine.
*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "prim.h"
#include "lex.h"
#include "splay.h"
#include "env.h"

static S env;

static void init() {
  // &env = Env_new();
  // Env_init(&env);
  // Prim_init(&env);
  Lex_init();
}

S *S_read() {
  return Lex_parseExpr();
}

S *S_eval(S *expr, S *env) {
  S *root, *cmd, *args;
  if (ATOMP(expr))
    return (expr->Symbol.type == Symbol)?
      // Map_lookup(env, expr->Symbol.val)
      expr:
      expr;
  root = expr;
  while (!NILP(expr)) {
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
  if (ATOMP(expr)) {
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
    for (expr = expr->Cons.cdr; !NILP(expr); expr = expr->Cons.cdr) {
      fprintf(stdout, ATOMP(expr->Cons.car)? " ": "");
      S_print(expr->Cons.car);
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
    S_print(S_eval(S_read(), &env));
    fprintf(stdout, "\n");
  }
  return 0;
}
