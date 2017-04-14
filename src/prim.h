/*
  paren primitive.
*/

#ifndef PRIM
#define PRIM

#include "ast.h"

#define NIL 0
#define T 1

#define SYMBOL 3
#define KEYWORD 4
#define CHARACTER 5
#define STRING 6
#define NUMBER 7
#define FUNCTION 9

#include "env.h"

extern void Prim_init(struct Env *env);

#endif
