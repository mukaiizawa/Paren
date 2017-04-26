/*
  paren primitive.
*/

#ifndef IS_LOADED_PRIM
#define IS_LOADED_PRIM

typedef enum {
  Cons,
  Symbol,
  Keyword,
  String,
  Character,
  Number,
  Function,
  Error
} Type;

typedef union S {
  struct {
    Type type;
    union S *car, *cdr;
  } Cons;
  struct {
    Type type;
    char *val;
  } Symbol;
  struct {
    Type type;
    char *val;
  } Keyword;
  struct {
    Type type;
    char *val;
  } String;
  struct {
    Type type;
    char val;
  } Character;
  struct {
    Type type;
    double val;
  } Number;
  struct {
    Type type;
    void *val;
  } Function;
  struct {
    Type type;
    char *val;
  } Error;
} S;

#include "env.h"

extern S *nil;
extern S *t;

extern void Prim_init(Env *env);
extern S *S_new(Type type, char *str);

extern S *read();
extern S *eval(S *s, Env *env);
extern S *print(S *s);
extern S *isNil(S *expr);
extern S *isAtom(S *expr);
extern S *cons(S *car, S *cdr);
extern S *reverse(S *expr);
// extern S *asString(S *expr);
extern S *plus(S *expr);

#endif
