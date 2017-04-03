/*
  abstruct syntax tree.
*/

#include <stdio.h>
#include <stdlib.h>

#include "ast.h"

// node
#define NODE 0
// leaf
#define   IDENTIFIER 1
#define   CHAR 2
#define   STRING 3
#define   NUMBER 4

typedef struct {
  int type, isCmd;
  char* val;
  struct AstNode *prev, *car, *cdr;
} AstNode;

AstNode *AstNode_isLeaf(AstNode *node) {
  return node->cdr == NULL;
}

// AstNode *AstNode_addLeaf(AstNode* node, int type, int isCmd, char* val) {
//   AstNode* newNode;
//   if ((newNode = (AstNode *)calloc(1, sizeof(AstNode))) == NULL ) {
//     fprintf(stderr, "AstNode_addLeaf: Cannnot allocate memory.");
//     exit(1);
//   }
//   newNode->type = type;
//   newNode->isCmd = isCmd;
//   newNode->val = val;
//   node = newNode;
//   return node;
// }

// AstNode* AstNode_cons(AstNode *node1, AstNode *node2) {
//   node1->cdr = node2;
//   node2->prev = node1;
//   return node1;
// }

void AstNode_dump(AstNode *node) {
  if (AstNode_isLeaf(node)) {
    // TODO: switch node type...
    printf("%s", node->val);
    return;
  }
  printf("(%s", node->car->val);
  // AstNode *next;
  // for (next = node->cdr; !AstNode_isLeaf(next); next = next->cdr) {
  //   if (AstNode_isLeaf(next))
  //     printf(" %s", next->car->val);
  //   else
  //     AstNode_dump(next);
  // }
  // printf(")");
}

int main(void) {
  // AstNode n1, n2, l1;
  // l1.val = "list";
  // n1.car = &l1;
  return 0;
}
