/*
  paren main routine.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "lex.h"
#include "ast.h"
#include "env.h"
#include "prim.h"

static struct Env env;

void Paren_init() {
  Env_init(&env);
  Prim_init(&env);
  Lex_init();
}

struct Ast *read() {
  return Lex_parse();
}

struct Ast *eval(struct Ast *ast) {
  struct Ast *cmd, *ope;
  cmd = ast->car;
  ope = ast->cdr;
  if (Ast_isLeaf(ast))
    return ast;
  if (!Ast_isLeaf(cmd))
    cmd->val = eval(cmd);
  return NULL;
}

void print(struct Ast *ast) {
  if (ast->type == NUMBER)
    printf(" %f", *(double *)ast->val);
  else if (ast->type == CHARACTER)
    printf(" %c", *(char *)ast->val);
  else 
    printf(" %s", (char *)ast->val);
}

int main(int argc, char* argv[]) {
  Paren_init();
  while (1) {
    fputs(") ", stdout);
    print(eval(read()));
    fflush(stdout);
  }
  return 0;
}
