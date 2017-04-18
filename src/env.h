/*
  environment.
*/

#ifndef ENV
#define ENV

struct Env {
  struct EnvNode {
    char *key;
    int type;
    struct Object *obj;
    void *val;
    struct EnvNode *next;
  } *head;
  struct Env *outer;
};

extern struct Env *Env_new();
extern void Env_init(struct Env *env);
extern int Env_isNil(struct Env *env);
extern int EnvNode_isNil(struct EnvNode *node);
extern struct EnvNode *EnvNode_alloc();
extern void Env_push(struct Env *env);
extern void Env_install(struct Env *env, struct Object *obj);
extern struct EnvNode *Env_lookup(struct Env *env, char *key);

#endif
