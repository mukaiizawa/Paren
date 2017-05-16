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

// static Splay specialTable;
static Env env;

// static S *Special_if(S *expr) {
//   int n;
//   if ((n = LENGTH(expr)) <= 1 || n > 3)
//     Error_new("if: Illegal argument exception.");
//   if(NILP(S_eval(FIRST(expr))))
//     return (n == 2)?
//       nil:
//       S_eval(THIRD(expr));
//   return S_eval(SECOND(expr));
// }

// static S *Special_quote(S *expr) {
//   return expr;
// }

static void Paren_init() {
  // Splay_init(&specialTable);
  // Splay_put(&specialTable, "if", Special_new(Special_if));
  // Splay_put(&specialTable, "quote", Special_new(Special_quote));
  Prim_initSymbolTable(Env_init(&env));
  Lex_init();
}

extern S *S_print(S *expr);

static S *S_errorHandler(S *expr) {
  S_print(expr);
  exit(1);
}

S *S_read() {
  return Lex_parseExpr();
}

S *S_eval(S *expr, Env *env) {
  S *root, *cmd, *args;
  if (ATOMP(expr)) {
    root = S_isType(expr, Symbol)?
      (S *)Env_get(env, expr->Symbol.val, Error_new("eval: undefined variable.")):
      expr;
    return S_isType(root, Error)?
      S_errorHandler(expr):
      expr;
  }
  // if ((S *)(cmd = Splay_get(FIRST(expr)->Symbol.val)) != NULL)
  //   return (cmd->Function.f)(expr);
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
    if (S_isType(expr, Number)) {
      double intptr, fraction;
      fraction = modf(expr->Number.val, &intptr);
      if (fraction == 0)
        printf("%d", (int)intptr);
      else
        printf("%f", expr->Number.val);
    }
    else if (S_isType(expr, Character))
      printf("%c", expr->Character.val);
    else if (S_isType(expr, Function))
      printf("%d", expr->Cons.type);
    else if (S_isType(expr, Keyword))
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
  Paren_init();
  while (1) {
    printf(") ");
    S_print(S_eval(S_read(), &env));
    printf("\n");
  }
  return 0;
}
