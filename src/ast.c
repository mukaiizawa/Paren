/*
  abstruct syntax tree.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "ast.h"

int Ast_isLeaf(struct Ast *node) {
  return node->cdr == node;
}

int Ast_isNil(struct Ast *node) {
  return node->car == node;
}

struct Ast *Ast_alloc() {
  struct Ast *node;
  if ((node = (struct Ast *)calloc(1, sizeof(struct Ast))) == NULL) {
    fprintf(stderr, "Ast_alloc: Cannot allocate memory.");
    exit(1);
  }
  return node->car = node->cdr = node->prev = node;
}

struct Ast *Ast_cons(struct Ast *car, struct Ast *cdr) {
  struct Ast *prev;
  if (Ast_isLeaf(cdr) && !Ast_isNil(cdr)) {
    fprintf(stderr, "Ast_cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  prev = Ast_alloc();
  prev->car = car;
  prev->cdr = cdr;
  car->prev = cdr->prev = prev;
  return  prev;
}

struct Ast *Ast_consWithNil(struct Ast *car) {
  struct Ast *nil;
  nil = Ast_alloc();
  return Ast_cons(car, nil);
}

void Ast_dump(struct Ast *node) {
  struct Ast *next;
  if (Ast_isLeaf(node)) {
    printf("%s", node->val);
    return;
  }
  printf("(");
  Ast_dump(node->car);
  for (next = node->cdr; !Ast_isNil(next); next = next->cdr) {
    if (Ast_isLeaf(next->car))
      printf(" %s", next->car->val);
    else {
      printf(" ");
      Ast_dump(next->car);
    }
  }
  printf(")\n");
}
