/*
  environment.
*/

#ifndef IS_LOADED_ENV
#define IS_LOADED_ENV

#include "prim.h"

typedef struct Env {
  struct EnvNode {
    char *key;
    S *expr;
    struct EnvNode *next;
  } *head;
  struct Env *outer;
} Env;

extern Env *Env_new();
extern void Env_init(Env *env);
extern int Env_isNil(Env *env);
extern int EnvNode_isNil(struct EnvNode *node);
extern void Env_push(Env *env);
extern void Env_pop(Env *env);
extern void Env_install(Env *env, char *key, S *expr);
extern S *Env_lookup(Env *env, char *key);

#endif
