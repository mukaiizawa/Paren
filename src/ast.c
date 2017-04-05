/*
  abstruct syntax tree.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "ast.h"

int S_isAtom(struct S *node) {
  return !(node->type == CONS);
}

int S_isNil(struct S *node) {
  return node->type == NIL;
}

struct S *S_alloc() {
  struct S *p;
  if ((p = (struct S *)calloc(1, sizeof(struct S))) == NULL) {
    fprintf(stderr, "S_alloc: Cannot allocate memory.");
    exit(1);
  }
  return p;
}

struct S *S_cons(struct S *car, struct S *cdr) {
  struct S *prev;
  if (cdr->type != CONS && !S_isNil(cdr)) {
    fprintf(stderr, "S_cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  prev = S_alloc();
  prev->type = CONS;
  prev->car = car;
  prev->cdr = cdr;
  car->prev = cdr->prev = prev;
  return  prev;
}

struct S *S_consNil(struct S *car) {
  struct S *nil;
  nil = S_alloc();
  nil->type = NIL;
  return S_cons(car, nil);
}

void S_dump(struct S *node) {
  struct S *next;
  if (S_isAtom(node)) {
    printf("%s", node->val);
    return;
  }
  printf("(");
  S_dump(node->car);
  for (next = node->cdr; !S_isNil(next); next = next->cdr) {
    if (S_isAtom(next->car))
      printf(" %s", next->car->val);
    else {
      printf(" ");
      S_dump(next->car);
    }
  }
  printf(")");
}
