#include <stdio.h>
#include <assert.h>

#include "lex.h"
#include "prim.h"

int main(void) {
  Env *env;
  S *expr;
  int i = 1, j = 2, k = 3;

  env = Env_new();
  Env_init(env);

  // not found.
  expr = Env_lookup(env, "i");
  assert(expr == NULL);

  // found i.
  expr = S_newExpr(Number, "1");
  Env_install(env, "i", expr);
  expr = Env_lookup(env, "i");
  assert(expr != NULL && expr->Atom.number == 1);

  // found i.
  expr = S_newExpr(Number, "2");
  Env_install(env, "j", expr);
  expr = Env_lookup(env, "i");
  assert(expr != NULL && expr->Atom.number == 1);

  // create next generation.
  Env_push(env);

  // install k.
  expr = S_newExpr(Number, "3");
  Env_install(env, "k", expr);
  expr = Env_lookup(env, "k");
  assert(expr != NULL && expr->Atom.number == 3);

  // install new i.
  expr = S_newExpr(Number, "10");
  Env_install(env, "i", expr);
  expr = Env_lookup(env, "i");
  assert(expr != NULL && expr->Atom.number == 10);

  return 0;
}
