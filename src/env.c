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
    int type;
    void *val;
    struct EnvNode *prev, *next;
  } *node;
};

int Env_isRoot(struct Env *env) {
  return env->top == env;
}

int Env_isBotommost(struct Env *env) {
  return env->bottom == NULL;
}

int EnvNode_isLast(struct EnvNode *envNode) {
  return envNode->next == NULL;
}

struct Env *Env_bottommost(struct Env *env) {
  while (!Env_isBotommost(env))
    env = env->bottom;
  return env;
}

struct Env *Env_add(struct Env *env) {
  struct Env *new;
  if ((new = (struct Env *)calloc(1, sizeof(struct Env))) == NULL) {
    fprintf(stderr, "Env_add: Cannot allocate memory.");
    exit(1);
  }
  env = Env_bottommost(env);
  env->bottom = new;
  new->top = env;
  new->bottom = NULL;
  return new;
}

struct Env *Env_addNode(struct Env *env, char *key, int type, void *val) {
  struct EnvNode *node;
  if ((node = (struct EnvNode *)calloc(1, sizeof(struct EnvNode))) == NULL) {
    fprintf(stderr, "Env_addNode: Cannot allocate memory.");
    exit(1);
  }
  node->key = key;
  node->type = type;
  node->val = val;
  env->node->prev = node;
  node->next = env->node;
  env->node = node;
  return env;
}

struct EnvNode *Env_lookup(struct Env *env, char *key) {
  struct EnvNode *node;
  env = Env_bottommost(env);
  while (!Env_isRoot(env)) {
    node = env->node;
    while (!EnvNode_isLast(node)) {
      if (strcmp(node->key, key) == 0) {
        return node;
      }
    }
    env = env->top;
  }
  return NULL;
}

int main(void) {
  struct Env env;
  struct EnvNode *node;
  Env_add(&env);
  int i = 3;
  Env_addNode(&env, "test", 3, &i);
  node = Env_lookup(&env, "test");
  if (node != NULL) {
    printf("%d", *(int *)node->val);
  }
  return 0;
}
