/*
  environment.
*/

#ifndef IS_LOADED_ENV
#define IS_LOADED_ENV

typedef struct Env {
  struct EnvNode {
    char *key;
    S *val;
    struct EnvNode *next;
  } *head;
  struct Env *outer;
} Env;

extern Env *Env_new();
extern void Env_init(Env *env);
extern void Env_push(Env *env);
extern void Env_pop(Env *env);
extern void Env_install(Env *env, char *key, S *val);
extern S *Env_lookup(Env *env, char *key);

#endif
