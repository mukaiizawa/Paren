/*
  s-expression.
*/

#include <stdio.h>
#include <stdlib.h>

#include "sexpr.h"

extern S *t;
extern S *nil;

char *TypeString[10] = {
  "Cons",
  "Map",
  "Symbol",
  "Keyword",
  "String",
  "Character",
  "Number",
  "Function",
  "Stream",
  "Error"
};

static S *S_alloc() {
  S *expr;
  if ((expr = (S *)calloc(1, sizeof(S))) == NULL) {
    fprintf(stderr, "S_alloc: Cannot allocate memory.");
    exit(1);
  }
  return expr;
}

S *Cons_new(S *car, S *cdr) {
  S *prev;
  if (ATOMP(cdr) && !NILP(cdr)) {
    fprintf(stderr, "Cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  prev = S_alloc();
  prev->Cons.type = Cons;
  FIRST(prev) = car;
  REST(prev) = cdr;
  return prev;
}

static struct MapNode *MapNode_alloc() {
  struct MapNode *mapNode;
  if ((mapNode = (struct MapNode *)calloc(1, sizeof(struct MapNode))) == NULL) {
    fprintf(stderr, "MapNode_alloc: Cannot allocate memory.");
    exit(1);
  }
  return mapNode;
}

S *Map_new(S *car, S *cdr) {
  S *expr;
  expr = S_alloc();
  expr->Map.type = Map;
  expr->Map.head = MapNode_alloc();
  return expr;
}

S *Symbol_new(char *val) {
  S *expr;
  expr = S_alloc();
  expr->Symbol.type = Symbol;
  expr->Symbol.val = val;
  return expr;
}

S *Keyword_new(char *val) {
  S *expr;
  expr = S_alloc();
  expr->Keyword.type = Keyword;
  expr->Keyword.val = val;
  return expr;
}

S *String_new(char *val) {
  S *expr;
  expr = S_alloc();
  expr->String.type = String;
  expr->String.val = val;
  return expr;
}

S *Character_new(char val) {
  S *expr;
  expr = S_alloc();
  expr->Character.type = Character;
  expr->Character.val = val;
  return expr;
}

S *Number_new(double val) {
  S *expr;
  expr = S_alloc();
  expr->Number.type = Number;
  expr->Number.val = val;
  return expr;
}

S *Function_new(S *f(S *), S *args) {
  S *expr;
  expr = S_alloc();
  expr->Function.type = Function;
  expr->Function.f = f;
  expr->Function.args = args;
  return expr;
}

S *Stream_new(FILE *stream) {
  S *expr;
  expr = S_alloc();
  expr->Stream.type = Stream;
  expr->Stream.stream = stream;
  return expr;
}

S *Error_new(char *str) {
  S *expr;
  expr = S_alloc();
  expr->Error.type = Error;
  expr->Error.val = str;
  return expr;
}
