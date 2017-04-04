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

struct S *S_cons(struct S *car, struct S *cdr) {
  struct S *prev;
  if (cdr->type != CONS && cdr != NIL) {
    fprintf(stderr, "S_cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  if ((prev = (struct S *)calloc(1, sizeof(struct S))) == NULL) {
    fprintf(stderr, "S_cons: Cannot allocate memory.");
    exit(1);
  }
  prev->type = CONS;
  prev->car = car;
  prev->cdr = cdr;
  car->prev = cdr->prev = prev;
  return  prev;
}

void S_dump(struct S *node) {
  struct S *next;
  if (S_isAtom(node)) {
    printf("%s", node->val);
    return;
  }
  printf("(%s", node->car->val);
  for (next = node->cdr; next != NIL; next = next->cdr) {
    if (S_isAtom(next->car))
      printf(" %s", next->car->val);
    else {
      printf(" ");
      S_dump(next->car);
    }
  }
  printf(")");
}
