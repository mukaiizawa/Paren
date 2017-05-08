/*
  paren primitive.
*/

#ifndef IS_LOADED_PRIM
#define IS_LOADED_PRIM

typedef enum {
  Cons,
  Map,
  Symbol,
  Keyword,
  String,
  Character,
  Number,
  Function,
  Stream,
  Error
} Type;

typedef union S {
  struct {
    Type type;
    union S *car, *cdr;
  } Cons;
  struct {
    Type type;
    struct MapNode {
      union S *key;
      union S *val;
      struct MapNode *next;
    } *head;
  } Map;
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
    union S *args;
    union S *(* f)(union S *);
  } Function;
  struct {
    Type type;
    FILE *stream;
  } Stream;
  struct {
    Type type;
    char *val;
  } Error;
} S;

#include "env.h"

extern void Prim_init(Env *env);

extern S *Cons_new(S *car, S *cdr);
// extern S *Map_new(S *car, S *cdr);
extern S *Symbol_new(char *val);
extern S *Keyword_new(char *val);
extern S *String_new(char *val);
extern S *Character_new(char val);
extern S *Number_new(double val);
extern S *Function_new(S *f(S *), S *args);
extern S *Stream_new(FILE *stream);
extern S *Error_new(char *val);
extern int S_isAtom(S *expr);
extern int S_isNil(S *expr);

extern S *t;
extern S *nil;
extern S *in;
extern S *out;
extern S *err;

extern S *car(S *expr);
extern S *cdr(S *expr);
extern S *first(S *expr);
extern S *second(S *expr);
extern S *third(S *expr);
extern S *isNil(S *expr);
extern S *isAtom(S *expr);
extern S *cons(S *expr);
extern S *list(S *expr);
extern S *length(S *expr);
extern S *reverse(S *expr);
// extern S *plus(S *expr);
// extern S *asString(S *expr);
extern S *dump(S *expr);

#endif
