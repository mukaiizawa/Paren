/*
  abstruct syntax tree.
*/

#include <stdio.h>
#include <stdlib.h>

#include "ast.h"

struct Ast *Ast_new() {
  struct Ast *node;
  if ((node = (struct Ast *)calloc(1, sizeof(struct Ast))) == NULL) {
    fprintf(stderr, "Ast_new: Cannot allocate memory.");
    exit(1);
  }
  return node->car = node->cdr = node->prev = node;
}

struct Ast *Ast_reverse(struct Ast *ast) {
  struct Ast *acc;
  acc = Ast_new();
  for (; !Ast_isNil(ast); ast = REST(ast)) {
    acc = Ast_cons(FIRST(ast), acc);
  }
  return acc;
}
