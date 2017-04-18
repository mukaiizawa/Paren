#include <stdio.h>
#include <assert.h>

#include "env.h"

int main(void) {
  struct Env *env;
  struct EnvNode *node;
  struct Object *obj;
  int i = 1, j = 2, k = 3;

  env = Env_new();
  Env_init(env);

  // not found.
  node = Env_lookup(env, "i");
  assert(node == NULL);

  // found i.
  obj = Object_new(INTEGER, "1");
  Env_install(env, "i", obj);
  node = Env_lookup(env, "i");
  assert(node != NULL && node->obj->val.integer == 1);

  // // found i.
  // Env_install(env, "j", 0, &j);
  // node = Env_lookup(env, "i");
  // assert(node != NULL && (*(int *)node->val == 1));
  //
  // // create next generation.
  // Env_push(env);
  //
  // // install k.
  // Env_install(env, "k", 0, &k);
  // node = Env_lookup(env, "k");
  // assert(node != NULL && (*(int *)node->val == 3));
  //
  // // update i.
  // i = 10;
  // Env_install(env, "i", 0, &i);
  // node = Env_lookup(env, "i");
  // assert(node != NULL && (*(int *)node->val == 10));

  return 0;
}
