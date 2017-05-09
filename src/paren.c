/*
  paren main routine.
*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "sexpr.h"
#include "prim.h"
#include "lex.h"
#include "env.h"

static S *env;

static void init() {
  // env = Env_new();
  // Env_init(env);
  // Prim_init(env);
  Lex_init();
}

int main(int argc, char* argv[]) {
  init();
  while (1) {
    fprintf(stdout, ") ");
    S_print(S_eval(S_read(), env));
    fprintf(stdout, "\n");
  }
  return 0;
}
