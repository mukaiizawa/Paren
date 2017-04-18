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

static struct Ast *read() {
  return Lex_parse();
}

static struct Ast *eval(struct Ast *ast) {
  struct Ast *first, *rest;
  if (Ast_isLeaf(ast))
    return ast;
  first = ast->car;
  rest = ast->cdr;
  if (!Ast_isLeaf(first))
    first = eval(first);
  if (first->obj->type != FUNCTION) {
    fprintf(stderr, "eval: '%s' is not a function.\n", asString(first->obj));
    return NULL;
  }
  // Env_lookup();
  return NULL;
}

static void print(struct Ast *ast) {
  int type;
  if (ast == NULL) {
    return;
  }
  if (Ast_isLeaf(ast)) {
    if ((type = ast->obj->type) == INTEGER)
      fprintf(stdout, " %d", ast->obj->val.integer);
    else if (type == DOUBLE)
      fprintf(stdout, " %f", ast->obj->val.dfloat);
    else if (type == CHARACTER)
      fprintf(stdout, " %c", ast->obj->val.character);
    else if (type == STRING)
      fprintf(stdout, " %s", ast->obj->val.string);
    else {
      fprintf(stderr, "print: Unknown type.");
    }
    fprintf(stdout, "\n");
    return;
  }
  fprintf(stdout, "(");
  print(ast->car);
  for (ast = ast->cdr; !Ast_isNil(ast); ast = ast->cdr) {
    if (!Ast_isLeaf(ast->car))
      fprintf(stdout, " ");
    print(ast->car);
  }
  fprintf(stdout, "\n");
  fflush(stdout);
}

int main(int argc, char* argv[]) {
  Paren_init();
  while (1) {
    fputs(") ", stdout);
    print(eval(read()));
  }
  return 0;
}
