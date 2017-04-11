/*
  paren main routine.
*/

#include <stdio.h>
#include <string.h>

#include "lex.h"
#include "ast.h"

struct Ast *read() {
  return Lex_parse();
}

char *eval(struct Ast *ast) {
  Ast *cmd *ope;
  cmd = ast->car;
  ope = ast->cdr;
  if (Ast_isLeaf(ast))
    return ast->val;
  if (!Ast_isLeaf(cmd))
    cmd = eval(cmd);
  if (strcmp(cmd, "+") == 0) {
    int i;
    i = 0;
    while (!Ast_isNil(ope)) {
      i = i + eval(ope->car);
      cmd = atoi(ope)
      ope = ope->cdr;
    }
    return sprintf(ast->val, 
  }
  return "eval";
}

char *print() {
  return "print";
}

int main(int argc, char* argv[]) {
  Lex_init();
  while (1) {
    fputs(") ", stdout);
    printf("%s", eval(read()));
    fflush(stdout);
  }
  return 0;
}
