#include <string.h>
#include <assert.h>
#include "ast.h"

int main(void) {
  struct S node0, node1, node2, node3;
  node0.type = STRING;
  node0.val = "list";
  node1.type = STRING;
  node1.val = "1";
  node2.type = STRING;
  node2.val = "2";
  node3.type = STRING;
  node3.val = "3";
  S_dump(S_cons(&node0, NIL));
  // S_dump(S_cons(&node0, S_cons(&node1, S_cons(S_cons(&node2, S_cons(&node3, NIL)), NIL))));
  return 0;
}
