#include <stdio.h>

#include "ast.h"

int main(void) {
  struct Ast *root, *node0, *node1, *node2, *node3;
  node0 = Ast_alloc();
  node0->val = "list";
  node1 = Ast_alloc();
  node1->val = "1";
  node2 = Ast_alloc();
  node2->val = "2";
  node3 = Ast_alloc();
  node3->val = "3";
  root = Ast_cons(
      node0,
      Ast_cons(
        Ast_cons(
          node1,
          Ast_consWithNil(node2)),
        Ast_consWithNil(node3)));

  printf("\n) root\n");
  Ast_dump(root);
  printf(" ; <=> (list (1 2) 3)\n");

  printf("\n) (first root)\n");
  Ast_dump(FIRST(root));
  printf(" ; <=> list\n");

  printf("\n) (rest root)\n");
  Ast_dump(REST(root));
  printf(" ; <=> ((1 2) 3)\n");

  printf("\n) (second root)\n");
  Ast_dump(SECOND(root));
  printf(" ; <=> (1 2)\n");

  printf("\n) (third root)\n");
  Ast_dump(THIRD(root));
  printf(" ; <=> 3\n");
  return 0;
}
