#include <stdio.h>

#include "ast.h"
#include "prim.h"

void Ast_dump(struct Ast *node) {
  struct Ast *next;
  if (Ast_isLeaf(node)) {
    printf("%s", node->val);
    return;
  }
  printf("(");
  Ast_dump(node->car);
  for (next = node->cdr; !Ast_isNil(next); next = next->cdr) {
    if (Ast_isLeaf(next->car)) {
      if (node->type == NUMBER)
        printf(" %f", *(double *)node->val);
      else if (node->type == CHARACTER)
        printf(" %c", *(char *)node->val);
      else 
        printf(" %s", (char *)node->val);
    }
    else {
      printf(" ");
      Ast_dump(next->car);
    }
  }
  printf(")");
}

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
          Ast_cons(node2, Ast_alloc())),
        Ast_cons(node3, Ast_alloc())));

  printf("\n) root\n");
  Ast_dump(root);
  printf(" ; <=> (list (1 2) 3)");

  printf("\n) (first root)\n");
  Ast_dump(FIRST(root));
  printf(" ; <=> list");

  printf("\n) (rest root)\n");
  Ast_dump(REST(root));
  printf(" ; <=> ((1 2) 3)");

  printf("\n) (second root)\n");
  Ast_dump(SECOND(root));
  printf(" ; <=> (1 2)");

  printf("\n) (third root)\n");
  Ast_dump(THIRD(root));
  printf(" ; <=> 3");
  return 0;
}
