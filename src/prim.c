/*
  paren primitive.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "macro.h"
#include "prim.h"
#include "lex.h"

defalloc(S);

struct S *read() {
  return Lex_parseS();
}

struct S *eval(struct S *ast) {
  struct S *first, *rest;
  if (S_isLeaf(ast))
    return ast;
  first = ast->car;
  rest = ast->cdr;
  if (!S_isLeaf(first))
    first = eval(first);
  if (first->obj->type != FUNCTION) {
    fprintf(stderr, "eval: '%s' is not a function.\n", asString(first->obj)->val.string);
    return NULL;
  }
  // Env_lookup();
  return NULL;
}

void print(struct S *ast) {
  int type;
  if (ast == NULL) {
    return;
  }
  if (S_isLeaf(ast)) {
    fprintf(stdout, "%s\n", asString(ast->obj)->val.string);
    return;
  }
  fprintf(stdout, "(");
  print(ast->car);
  for (ast = ast->cdr; !S_isNil(ast); ast = ast->cdr) {
    if (!S_isLeaf(ast->car))
      fprintf(stdout, " ");
    print(ast->car);
  }
  fprintf(stdout, "\n");
  fflush(stdout);
}

int isAtom(struct S *s) {
  return s->Atom.type !=Cons;
}

int isNil(struct S *s) {
  return s->Atom.type == Nil;
}

struct S *cons(struct S *car, struct S *cdr) {
  struct S *prev;
  if (isAtom(cdr) && !isNil(cdr)) {
    fprintf(stderr, "Cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  prev = S_alloc();
  prev->Cons->car = car->Cons;
  prev->Cons->cdr = cdr->Cons;
  car->Cons->prev = cdr->Cons->prev = prev->Cons;
  return prev;
}

struct S *S_newNil() {
  struct S *s;
  s = S_alloc();
  s->Cons.type = Nil;
  return s;
}

struct S *S_newNil() {
  struct S *s;
  s = S_alloc();
  s->Cons.type = Nil;
  return s;
}

struct S *S_newSymbol(char *str) {
  struct S *s;
  s = S_alloc();
  s->Atom.type = Symbol;
  s->Atom.string = str;
  return s;
}

struct S *S_newKeyword(char *str) {
  struct S *s;
  s = S_alloc();
  s->Atom.type = Keyword;
  s->Atom.string = str;
  return s;
}

struct S *S_newString(char *str) {
  struct S *s;
  s = S_alloc();
  s->Atom.type = String;
  s->Atom.string = str;
  return s;
}

struct S *S_newCharacter(char *str) {
  struct S *s;
  s = S_alloc();
  s->Atom.type = Character;
  s->Atom.string = str;
  s->Atom.character = str[0];
  return s;
}

struct S *S_newNumber(char *str) {
  struct S *s;
  s = S_alloc();
  s->Atom.type = Number;
  s->Atom.string = str;
  s->Atom.string = str;
  return s;
}

struct S *asString(struct S *s) {
  struct S *new;
  new = S_alloc();
  new->Atom.type = STRING;
  new->Atom.string = s.Atom.string;
  return new;
}
