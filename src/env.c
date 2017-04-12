/*
  environment.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Env {
  struct Env *prev, *next;
  struct EnvNode {
    char *key;
    char *type;
    void *val;
    struct EnvNode *prev, *next;
  } *node;
};

int Env_isRootEnv(struct Env *env) {
  return env->prev == env;
}

int Env_isLastEnvNode(struct Env *env, struct EnvNode *envNode) {
  return env == envNode->next;
}

struct EnvNode *Env_lookup(struct Env *env, char *key) {
  EnvNode node;
  while (!Env_isRootEnv(env)) {
    node = env->node;
    while (!Env_isLastEnvNode(node)) {
      if (strcmp(node->key, 
    }
  }
  return NULL;
}
