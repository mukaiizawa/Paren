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

int main(int argc, char* argv[]) {
  Paren_init();
  void *expr;
  Paren_prompt();
  while ((expr = S_read(&env, stdin)) != NULL) {
    S_print(S_eval(expr, &env));
    printf("\n");
    Paren_prompt();
  }
  return 0;
}
