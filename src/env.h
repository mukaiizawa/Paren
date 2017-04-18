/*
  environment.
*/

#ifndef ENV
#define ENV

#include "prim.h"

struct Env {
  struct EnvNode {
    char *key;
    struct Object *obj;
    struct EnvNode *next;
  } *head;
  struct Env *outer;
};

extern struct Env *Env_new();
extern void Env_init(struct Env *env);
extern int Env_isNil(struct Env *env);
extern int EnvNode_isNil(struct EnvNode *node);
extern void Env_push(struct Env *env);
extern void Env_install(struct Env *env, char *key, struct Object *obj);
extern struct EnvNode *Env_lookup(struct Env *env, char *key);

#endif
