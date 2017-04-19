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

static struct Env *env;

static void init() {
  env = Env_new();
  Env_init(env);
  Prim_init(env);
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
    fprintf(stderr, "eval: '%s' is not a function.\n", asString(first->obj)->val.string);
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
    fprintf(stdout, "%s\n", asString(ast->obj)->val.string);
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
  init();
  while (1) {
    fputs(") ", stdout);
    print(eval(read()));
  }
  return 0;
}
