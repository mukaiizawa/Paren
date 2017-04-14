/*
  paren primitive.
*/

#ifndef PPRIM
#define PPRIM

#include "ast.h"

#define NIL 0
#define T 1

#define SYMBOL 3
#define KEYWORD 4
#define CHARACTER 5
#define STRING 6
#define NUMBER 7


extern char *Paren_plus(struct Ast *ast);

#endif
