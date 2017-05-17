/*
  paren environment.
*/

typedef struct Env {
  struct EnvNode {
    struct EnvNode *outer;
    Splay map;
  } *root;
  Splay special;
} Env;

extern void Env_init(Env *env);
extern void Env_push(Env *env);
extern void Env_pop(Env *env);
extern void *Env_get(Env *env, char *key, void *orElse);
extern void Env_put(Env *env, char *key, void *val);
extern void *Env_getSpecial(Env *env, char *key);
extern void Env_putSpecial(Env *env, char *key, void *val);
extern void Env_free(Env *env);
