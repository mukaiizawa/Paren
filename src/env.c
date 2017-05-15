/*
   paren environment.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "splay.h"
#include "env.h"

static struct EnvNode *EnvNode_new() {
  struct EnvNode *node;
  if ((node = (struct EnvNode *)malloc(sizeof(struct EnvNode))) == NULL) {
    fprintf(stderr, "EnvNode_new: Cannot allocate memory.");
    exit(1);
  }
  node->outer = NULL;
  Splay_init(&node->map);
  return node;
}

static void EnvNode_free(struct EnvNode *node) {
  Splay_free(&node->map);
  free(node);
}

void Env_init(Env *env) {
  env->root = EnvNode_new();
}

void Env_push(Env *env) {
  struct EnvNode *node;
  node = EnvNode_new();
  node->outer = env->root;
  env->root = node;
}

void Env_pop(Env *env) {
  struct EnvNode *node;
  node = env->root;
  env->root = node->outer;
  EnvNode_free(node);
}

void *Env_get(Env *env, char *key) {
  struct EnvNode *node;
  void *val;
  for (node = env->root; node != NULL; node = node->outer) {
    if ((val = Splay_get(&node->map, key)) != NULL)
      return val;
  }
  return NULL;
}

void Env_put(Env *env, char *key, void *val) {
  Splay_put(&env->root->map, key, val);
}

void Env_free(Env *env) {
  struct EnvNode *node, *outer;
  node = env->root;
  while (node != NULL) {
    outer = node->outer;
    EnvNode_free(node);
    node = outer;
  }
}
