/*
  paren main routine.
*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "splay.h"
#include "env.h"
#include "prim.h"
#include "lex.h"

static Env env;

static void init() {
  Env_init(&env);
  Prim_init(&env);
  Lex_init();
}

S *S_read() {
  return Lex_parseExpr();
}

S *S_eval(S *expr, Env *env) {
  S *root, *cmd, *args;
  if (ATOMP(expr))
    return (expr->Symbol.type == Symbol)?
      (S *)Env_get(env, expr->Symbol.val, Error_new("eval: undefined variable.")):
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
        printf("%d", (int)intptr);
      else
        printf("%f", expr->Number.val);
    }
    else if (expr->Cons.type == Character)
      printf("%c", expr->Character.val);
    else if (expr->Cons.type == Function)
      printf("%d", expr->Cons.type);
    else if (expr->Cons.type == Keyword)
      printf(":%s", expr->Keyword.val);
    else
      printf("%s", expr->String.val);
  }
  else {
    printf("(");
    S_print(expr->Cons.car);
    for (expr = expr->Cons.cdr; !NILP(expr); expr = expr->Cons.cdr) {
      printf(ATOMP(expr->Cons.car)? " ": "");
      S_print(expr->Cons.car);
    }
    printf(")");
  }
  fflush(stdout);
  return expr;
}

int main(int argc, char* argv[]) {
  init();
  while (1) {
    printf(") ");
    S_print(S_eval(S_read(), &env));
    printf("\n");
  }
  return 0;
}
