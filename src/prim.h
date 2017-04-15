/*
  paren primitive.
*/

#ifndef PRIM
#define PRIM

#include "ast.h"
#include "env.h"

#define NIL 0
#define T 1

#define SYMBOL 1
#define KEYWORD 2
#define CHARACTER 3
#define STRING 4
#define FUNCTION 6
#define INTEGER 7
#define DOUBLE 7

struct Object {
  int type;
  union {
    char *symbol;
    char *keyword;
    char character;
    char *string;
    int integer;
    double dfloat;
    struct Ast *function;
  } val;
}; 

extern struct Object *Object_alloc(int type, void *val);
extern void Prim_init(struct Env *env);

#endif
