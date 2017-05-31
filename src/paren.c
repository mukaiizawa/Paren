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

static Env env;

static void Paren_init() {
  Env_init(&env);
  Prim_init(&env);
}

static void Paren_prompt() {
  printf(") ");
}

static S *Paren_errorHander(S *expr) {
  return S_print(expr);
}

int main(int argc, char* argv[]) {
  Paren_init();
  void *expr;
  Paren_prompt();
  while (1) {
    expr = S_eval(S_read(stdin));
    if (TYPEP(expr, Error)) Paren_errorHander(expr);
    else S_print(expr);
    printf("\n");
    Paren_prompt();
  }
  return 0;
}
