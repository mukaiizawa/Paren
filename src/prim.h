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
    // struct Cons *prev;
    // struct Cons *car, *cdr;
    union S *prev;
    union S *car, *cdr;
  } *Cons;
  struct Atom {
    Type type;
    // struct Cons *prev;
    union S *prev;
    char *string;
    char character;
    double number;
  } *Atom;
} S;

#include "env.h"

extern S *S_newNil();
extern S *S_newSymbol(char *val);
extern S *S_newKeyword(char *str);
extern S *S_newString(char *str);
extern S *S_newCharacter(char *str);
extern S *S_newNumber(char *str);
extern S *asString(S *obj);

extern S *read();
extern S *eval(S *s, Env *env);
extern void print(S *s);
extern S *isAtom(S *expr);
extern S *isNil(S *expr);
extern S *cons(S *car, S *cdr);
extern S *reverse(S *expr);

#endif
