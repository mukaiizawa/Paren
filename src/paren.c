/*
  paren main routine.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "lex.h"
#include "ast.h"
#include "pprim.h"

struct Ast *read() {
  return Lex_parse();
}

char *eval(struct Ast *ast) {
  struct Ast *cmd, *ope;
  cmd = ast->car;
  ope = ast->cdr;
  if (Ast_isLeaf(ast))
    return ast->val;
  if (!Ast_isLeaf(cmd))
    cmd->val = eval(cmd);
  if (strcmp(cmd->val, "+") == 0) {
    int i;
    for (i = 0; !Ast_isNil(ope); ope = ope->cdr)
      i = i + atoi(eval(ope->car));
    if ((ast->val = (char *)malloc(sizeof(char) * (int)(i / 10 + 2))) == NULL) {
      fprintf(stderr, "+: Cannot allocate memory.");
      exit(1);
    }
    free(ast->val);
    sprintf(ast->val, "%i", i);
    return ast->val;
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
