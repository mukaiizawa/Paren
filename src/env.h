/*
  paren environment.
*/

typedef struct Env {
  struct EnvNode {
    struct EnvNode *outer;
    Splay symbol;
  } *root;
  Splay special;
  Splay function;
} Env;

extern void Env_init(Env *env);
extern void Env_push(Env *env);
extern void Env_pop(Env *env);
extern void *Env_getSymbol(Env *env, char *key, void *orElse);
extern void Env_putSymbol(Env *env, char *key, void *val);
extern void *Env_getSpecial(Env *env, char *key);
extern void Env_putSpecial(Env *env, char *key, void *val);
extern void *Env_getFunction(Env *env, char *key);
extern void Env_putFunction(Env *env, char *key, void *val);
extern void Env_free(Env *env);
