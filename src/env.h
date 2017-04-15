/*
  environment.
*/

#ifndef ENV
#define ENV

struct Env {
  struct EnvNode {
    char *key;
    int type;
    void *val;
    struct EnvNode *next;
  } *head;
  struct Env *outer;
};

extern void Env_init(struct Env *env);
extern int Env_isNil(struct Env *env);
extern int EnvNode_isNil(struct EnvNode *node);
extern struct Env *Env_alloc();
extern struct EnvNode *EnvNode_alloc();
extern void Env_push(struct Env *env);
extern void Env_install(struct Env *env, char *key, int type, void *val);
extern struct EnvNode *Env_lookup(struct Env *env, char *key);

#endif
