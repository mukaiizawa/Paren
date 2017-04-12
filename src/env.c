/*
  environment.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Env {
  struct Env *top, *bottom;
  struct EnvNode {
    char *key;
    char *type;
    void *val;
    struct EnvNode *prev, *next;
  } *node;
};

int Env_isRootEnv(struct Env *env) {
  return env->top == NULL;
}

int Env_isBotommostEnv(struct Env *env) {
  return env->bottom == env;
}

int Env_isLastEnvNode(struct Env *env, struct EnvNode *envNode) {
  return env->node == envNode->next;
}

struct Env *Env_bottommost(struct Env *env) {
  while (Env_isBotommostEnv(env))
    env = env->bottom;
  return env;
}

struct EnvNode *Env_lookup(struct Env *env, char *key) {
  struct EnvNode *node;
  env = Env_bottommost(env);
  while (!Env_isRootEnv(env)) {
    node = env->node;
    while (!Env_isLastEnvNode(env, node)) {
      if (strcmp(node->key, key) == 0) {
        return node;
      }
    }
    env = env->top;
  }
  return NULL;
}

int main(void) {
  return 0;
}
