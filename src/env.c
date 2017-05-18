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
  Splay_init(&node->symbol);
  return node;
}

static void EnvNode_free(struct EnvNode *node) {
  Splay_free(&node->symbol);
  free(node);
}

void Env_init(Env *env) {
  env->root = EnvNode_new();
  Splay_init(&env->type);
  Splay_init(&env->special);
  Splay_init(&env->function);
  Splay_init(&env->structure);
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

void *Env_getSymbol(Env *env, char *key, void *orElse) {
  struct EnvNode *node;
  void *val;
  for (node = env->root; node != NULL; node = node->outer) {
    if ((val = Splay_get(&node->symbol, key)) != NULL)
      return val;
  }
  return orElse;
}

void Env_putSymbol(Env *env, char *key, void *val) {
  Splay_put(&env->root->symbol, key, val);
}

void *Env_getType(Env *env, char *key) {
  return Splay_get(&env->type, key);
}

void Env_putType(Env *env, char *key, void *val) {
  Splay_put(&env->type, key, val);
}

void *Env_getSpecial(Env *env, char *key) {
  return Splay_get(&env->special, key);
}

void Env_putSpecial(Env *env, char *key, void *val) {
  Splay_put(&env->special, key, val);
}

void *Env_getFunction(Env *env, char *key) {
  return Splay_get(&env->function, key);
}

void Env_putFunction(Env *env, char *key, void *val) {
  Splay_put(&env->function, key, val);
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
