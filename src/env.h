/*
  paren environment.
*/

typedef struct Env {
  struct EnvNode {
    struct EnvNode *outer;
    Splay map;
  } *root;
} Env;

extern void Env_init(Env *env);
extern void Env_push(Env *env);
extern void Env_pop(Env *env);
extern S *Env_get(Env *env, char *key);
extern void Env_put(Env *env, char *key, S *val);
extern void Env_free(Env *env);
