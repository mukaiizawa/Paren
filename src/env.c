/*
   environment.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "env.h"

void Env_init(struct Env *env) {
  env->outer = NULL;
  env->head = NULL;
  Env_push(env);
}

int Env_isNil(struct Env *env) {
  return env == NULL;
}

int EnvNode_isNil(struct EnvNode *node) {
  return node == NULL;
}

struct Env *Env_alloc() {
  struct Env *new;
  if ((new = (struct Env *)calloc(1, sizeof(struct Env))) == NULL) {
    fprintf(stderr, "Env_alloc: Cannot allocate memory.");
    exit(1);
  }
  return new;
}

struct EnvNode *EnvNode_alloc() {
  struct EnvNode *new;
  if ((new = (struct EnvNode *)calloc(1, sizeof(struct EnvNode))) == NULL) {
    fprintf(stderr, "EnvNode_alloc: Cannot allocate memory.");
    exit(1);
  }
  return new;
}

void Env_push(struct Env *env) {
  if (env->head != NULL && env->head->next == NULL)
    return;
  struct Env *new;
  new = Env_alloc();
  new->head = EnvNode_alloc();
  new->head->next = NULL;
  new->outer = env->outer;
  env->outer = new;
}

void Env_install(struct Env *env, char *key, int type, void *val) {
  struct EnvNode *node;
  node = EnvNode_alloc();
  node->key = key;
  node->type = type;
  node->val = val;
  node->next = env->outer->head->next;
  env->outer->head->next = node;
}

struct EnvNode *Env_lookup(struct Env *env, char *key) {
  struct EnvNode *node;
  while (!Env_isNil(env = env->outer)) {
    node = env->head;
    while (!EnvNode_isNil(node = node->next)) {
      if (strcmp(node->key, key) == 0) {
        return node;
      }
    }
  }
  return NULL;
}
