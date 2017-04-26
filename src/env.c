/*
   environment.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "prim.h"
#include "env.h"

struct Env *Env_new() {
  struct Env *env;
  if ((env = (struct Env *)calloc(1, sizeof(struct Env))) == NULL) {
    fprintf(stderr, "Env_new: Cannot allocate memory.");
    exit(1);
  }
  return env;
}

static int Env_isNil(struct Env *env) {
  return env == NULL;
}

static int EnvNode_isNil(struct EnvNode *node) {
  return node == NULL;
}

void Env_init(struct Env *env) {
  env->outer = NULL;
  env->head = NULL;
  Env_push(env);
}

static struct EnvNode *EnvNode_alloc() {
  struct EnvNode *new;
  if ((new = (struct EnvNode *)calloc(1, sizeof(struct EnvNode))) == NULL) {
    fprintf(stderr, "EnvNode_alloc: Cannot allocate memory.");
    exit(1);
  }
  return new;
}

void Env_push(struct Env *env) {
  struct Env *new;
  new = Env_new();
  new->head = EnvNode_alloc();
  new->head->next = NULL;
  new->outer = env->outer;
  env->outer = new;
}

void Env_pop(struct Env *env) {
  // TODO
}

void Env_install(struct Env *env, char *key, S *val) {
  struct EnvNode *node;
  node = EnvNode_alloc();
  node->val = val;
  node->key = key;
  node->next = env->outer->head->next;
  env->outer->head->next = node;
}

S *Env_lookup(struct Env *env, char *key) {
  struct EnvNode *node;
  while (!Env_isNil(env = env->outer)) {
    node = env->head;
    while (!EnvNode_isNil(node = node->next)) {
      if (strcmp(node->key, key) == 0) {
        return node->val;
      }
    }
  }
   return S_new(Error, "eval: variable has no value.");
}
