/*
  abstruct syntax tree.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "ast.h"

int Ast_isAtom(struct Ast *node) {
  return !(node->type == AST_CONS);
}

int Ast_isNil(struct Ast *node) {
  return node->type == AST_NIL;
}

struct Ast *Ast_alloc() {
  struct Ast *p;
  if ((p = (struct Ast *)calloc(1, sizeof(struct Ast))) == NULL) {
    fprintf(stderr, "Ast_alloc: Cannot allocate memory.");
    exit(1);
  }
  return p;
}

struct Ast *Ast_cons(struct Ast *car, struct Ast *cdr) {
  struct Ast *prev;
  if (cdr->type != AST_CONS && !Ast_isNil(cdr)) {
    fprintf(stderr, "Ast_cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  prev = Ast_alloc();
  prev->type = AST_CONS;
  prev->car = car;
  prev->cdr = cdr;
  car->prev = cdr->prev = prev;
  return  prev;
}

struct Ast *Ast_consNil(struct Ast *car) {
  struct Ast *nil;
  nil = Ast_alloc();
  nil->type = AST_NIL;
  return Ast_cons(car, nil);
}

void Ast_dump(struct Ast *node) {
  struct Ast *next;
  if (Ast_isAtom(node)) {
    printf("%s", node->val);
    return;
  }
  printf("(");
  Ast_dump(node->car);
  for (next = node->cdr; !Ast_isNil(next); next = next->cdr) {
    if (Ast_isAtom(next->car))
      printf(" %s", next->car->val);
    else {
      printf(" ");
      Ast_dump(next->car);
    }
  }
  printf(")");
}
