/*
  abstruct syntax tree.
*/

#ifndef AST
#define AST

struct ast {
  int key;
  char *val;
  struct ast_node *left, *right;
};

#endif
