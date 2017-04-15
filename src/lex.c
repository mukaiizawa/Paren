/*
  lexical analysis.
*/

#include <stdio.h>
#include <stdlib.h>

#include "lex.h"
#include "ast.h"
#include "ahdrd.h"
#include "prim.h"

static struct Ahdrd ahdrd;

void Lex_init() {
  Ahdrd_init(&ahdrd, stdin);
}

struct Ast *Lex_parse() {
  return Lex_parseS();
}

struct Ast *Lex_parseAtom() {
  int c;
  struct Ast *atom;
  atom = Ast_alloc();
  if ((c = Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1)) == ':') {
    atom->type = KEYWORD;
    atom->val = Ahdrd_readKeyword(&ahdrd);
  }
  else if (c == '\'') {
    atom->type = CHARACTER;
    atom->val = Ahdrd_readCharacter(&ahdrd);
  }
  else if (c == '"') {
    atom->type = STRING;
    atom->val = Ahdrd_readString(&ahdrd);
  }
  else if (Ahdrd_isNumber(&ahdrd)) {
    atom->type = NUMBER;
    atom->val = Ahdrd_readSymbol(&ahdrd);
  }
  else {
    atom->type = SYMBOL;
    atom->val = Ahdrd_readSymbol(&ahdrd);
  }
  return atom;
}

struct Ast *Lex_parseS() {
  Ahdrd_readSpace(&ahdrd);
  struct Ast *ast;
  if (Ahdrd_peek(&ahdrd, 1) == '(') {
    Ahdrd_skipRead(&ahdrd);    // skip '('
    ast = Ast_alloc();
    while (Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1) != ')') {
      ast = Ast_cons(Lex_parseS(), ast);
    }
    Ahdrd_skipRead(&ahdrd);    // skip ')'
    return Ast_reverse(ast);
  }
  else
    return Lex_parseAtom();
}
