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

int main(int argc, char* argv[]) {
  Paren_init();
  while (1) {
    printf(") ");
    S_print(S_eval(S_read(&env, stdin), &env));
    printf("\n");
  }
  return 0;
}
