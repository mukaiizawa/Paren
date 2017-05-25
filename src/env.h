/*
  paren environment.
*/

typedef struct Env {
  struct EnvNode {
    struct EnvNode *outer;
    Splay symbol;
  } *root;
  Splay special;
  Splay keyword;
} Env;

extern void Env_init(Env *env);
extern void Env_push(Env *env);
extern void Env_pop(Env *env);
extern void *Env_getSymbol(Env *env, char *key);
extern void Env_putSymbol(Env *env, char *key, void *val);
extern void *Env_getSpecial(Env *env, char *key);
extern void Env_putSpecial(Env *env, char *key, void *val);
extern void *Env_getKeyword(Env *env, char *key);
extern void Env_putKeyword(Env *env, char *key, void *val);
extern void Env_free(Env *env);
