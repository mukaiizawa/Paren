#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "prim.h"

int main(void) {
  struct Ast *root, *node0, *node1, *node2, *node3;
  node0 = Ast_new();
  node0->obj = Object_new(STRING, "list");
  node1 = Ast_new();
  node1->obj = Object_new(STRING, "1");
  node2 = Ast_new();
  node2->obj = Object_new(STRING, "2");
  node3 = Ast_new();
  node3->obj = Object_new(STRING, "3");

  root = Ast_cons(
      node0,
      Ast_cons(
        Ast_cons(
          node1,
          Ast_cons(node2, Ast_new())),
        Ast_cons(node3, Ast_new())));
  // => (list (1 2) 3)

  assert(strcmp(FIRST(root)->obj->val.string, "list") == 0);
  assert(strcmp(FIRST(SECOND(root))->obj->val.string, "1") == 0);
  assert(strcmp(SECOND(SECOND(root))->obj->val.string, "2") == 0);
  assert(strcmp(THIRD(root)->obj->val.string, "3") == 0);

  return 0;
}
