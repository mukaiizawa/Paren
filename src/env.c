/*
   environment.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Env {
  struct EnvNode {
    char *key;
    int type;
    void *val;
    struct EnvNode *next;
  } *head;
  struct Env *next;
};

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

void Env_init(struct Env *env) {
  env = Env_alloc();
  env->next = NULL;
  env->head = EnvNode_alloc();
  env->head->next = NULL;
}

struct Env *Env_push(struct Env *env) {
  struct Env *new;
  Env_init(new);
  new->next = env;
  return new;
}

void Env_install(struct Env *env, char *key, int type, void *val) {
  struct EnvNode *node;
  node = EnvNode_alloc();
  node->key = key;
  node->type = type;
  node->val = val;
  node->next = env->head->next;
  env->head->next = node;
}

struct EnvNode *Env_lookup(struct Env *env, char *key) {
  struct EnvNode *node;
  while (!Env_isNil(env)) {
    node = env->head;
    while (!EnvNode_isNil((node = node->next))) {
      if (strcmp(node->key, key) == 0) {
        return node;
      }
    }
    env = env->next;
  }
  return NULL;
}

int main(void) {
  struct Env *env;
  struct EnvNode *node = NULL;
  int i = 200, j = 1000;
  Env_init(env);
  Env_install(env, "test", 3, &i);
  env = Env_push(env);
  Env_install(env, "tes", 3, &j);
  node = Env_lookup(env, "test");
  if (node != NULL)
    printf("Found %d.", *(int *)node->val);
  else
    printf("Not found.");
  return 0;
}
