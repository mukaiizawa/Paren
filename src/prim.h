/*
  paren primitive.
*/

#ifndef IS_LOADED_PRIM
#define IS_LOADED_PRIM

typedef enum {
  Symbol,
  Keyword,
  String,
  Character,
  Number,
  Function,
  Error,
  Cons
} Type;

typedef union S {
  struct Cons {
    Type type;
    union S *prev;
    union S *car, *cdr;
  } *Cons;
  struct Atom {
    Type type;
    union S *prev;
    char *string;
    char character;
    double number;
  } *Atom;
} S;

#include "env.h"

extern S *S_newExpr(Type type, char *str);
extern S *S_newNil();

extern S *read();
extern S *eval(S *s, Env *env);
extern void print(S *s);
extern S *isAtom(S *expr);
extern S *isNil(S *expr);
extern S *cons(S *car, S *cdr);
extern S *reverse(S *expr);
extern S *asString(S *obj);

#endif
