/*
   paren environment.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "std.h"
#include "splay.h"
#include "env.h"

static struct EnvNode *EnvNode_new() {
  struct EnvNode *node;
  node = xmalloc(sizeof(struct EnvNode));
  node->outer = NULL;
  Splay_init(&node->symbol, (int(*)(void*,void*))strcmp);
  return node;
}

static void EnvNode_free(struct EnvNode *node) {
  Splay_free(&node->symbol);
  free(node);
}

void Env_init(Env *env) {
  env->root = EnvNode_new();
  Splay_init(&env->keyword, (int(*)(void*,void*))strcmp);
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

void *Env_getSymbol(Env *env, char *key) {
  struct EnvNode *node;
  void *val;
  for (node = env->root; node != NULL; node = node->outer)
    if ((val = Splay_get(&node->symbol, key)) != NULL) return val;
  return NULL;
}

void Env_putSymbol(Env *env, char *key, void *val) {
  Splay_put(&env->root->symbol, key, val);
}

void *Env_getKeyword(Env *env, char *key) {
  return Splay_get(&env->keyword, key);
}

void Env_putKeyword(Env *env, char *key, void *val) {
  if (Splay_get(&env->keyword, key) != NULL) return;
  Splay_put(&env->keyword, key, val);
}
