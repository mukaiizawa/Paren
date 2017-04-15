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
  struct Ast *first, *rest;
  if (Ast_isLeaf(ast))
    return ast;
  first = ast->car;
  rest = ast->cdr;
  if (!Ast_isLeaf(first))
    first->obj = eval(first)->obj;
  return NULL;
}

void print(struct Ast *ast) {
  int type;
  if ((type = ast->obj->type) == INTEGER)
    printf(" %d", ast->obj->val.integer);
  else if (type == DOUBLE)
    printf(" %f", ast->obj->val.dfloat);
  else if (type == CHARACTER)
    printf(" %c", ast->obj->val.character);
  else if (type == STRING)
    printf(" %s", ast->obj->val.string);
  else
    return;
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
