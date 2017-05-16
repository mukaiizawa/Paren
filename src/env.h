/*
  paren environment.
*/

typedef struct Env {
  struct EnvNode {
    struct EnvNode *outer;
    Splay map;
  } *root;
} Env;

extern Env *Env_init(Env *env);
extern void Env_push(Env *env);
extern void Env_pop(Env *env);
extern void *Env_get(Env *env, char *key, void *orElse);
extern void Env_put(Env *env, char *key, void *val);
extern void Env_free(Env *env);
