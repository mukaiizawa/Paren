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

int main(int argc, char* argv[]) {
  init();
  while (1) {
    fprintf(stdout, ") ");
    print(eval(read(), env));
    fprintf(stdout, "\n");
  }
  return 0;
}
