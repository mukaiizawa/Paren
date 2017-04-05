#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "ast.h"

int main(void) {
  struct S *root, *node0, *node1, *node2, *node3;
  node0 = S_alloc();
  node0->type = STRING;
  node0->val = "list";
  node1 = S_alloc();
  node1->type = STRING;
  node1->val = "1";
  node2 = S_alloc();
  node2->type = STRING;
  node2->val = "2";
  node3 = S_alloc();
  node3->type = STRING;
  node3->val = "3";
  root = S_cons(
      node0,
      S_cons(
        S_cons(
          node1,
          S_consNil(node2)),
        S_consNil(node3)));

  printf("\nroot => ");
  S_dump(root);
  printf("\n<=> (list (1 2) 3)\n");

  printf("\n(first root) => ");
  S_dump(FIRST(root));
  printf("\n<=> list\n");

  printf("\n(rest root) => ");
  S_dump(REST(root));
  printf("\n<=> ((1 2) 3)\n");

  printf("\n(second root) => ");
  S_dump(SECOND(root));
  printf("\n<=> (1 2)\n");

  printf("\n(third root) => ");
  S_dump(THIRD(root));
  printf("\n<=> 3\n");
  return 0;
}
