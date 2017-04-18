/*
  lexical analysis.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lex.h"
#include "ast.h"
#include "ahdrd.h"
#include "prim.h"

static struct Ahdrd *ahdrd;

void Lex_init() {
  ahdrd = Ahdrd_new(stdin);
}

struct Ast *Lex_parse() {
  return Lex_parseS();
}

struct Ast *Lex_parseAtom() {
  int c;
  struct Ast *atom;
  atom = Ast_new();
  if ((c = Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1)) == ':') {
    atom->obj = Object_new(KEYWORD, Ahdrd_readKeyword(ahdrd));
  }
  else if (c == '\'') {
    atom->obj = Object_new(CHARACTER, Ahdrd_readCharacter(ahdrd));
  }
  else if (c == '"') {
    atom->obj = Object_new(STRING, Ahdrd_readString(ahdrd));
  }
  else if (Ahdrd_isNumber(ahdrd)) {
    char *token;
    atom->obj =  (strchr((token = Ahdrd_readNumber(ahdrd)), '.') != NULL)?
      Object_new(DOUBLE, token):
      Object_new(INTEGER, token);
  }
  else {
    atom->obj = Object_new(SYMBOL, Ahdrd_readSymbol(ahdrd));
  }
  return atom;
}

struct Ast *Lex_parseS() {
  struct Ast *ast;
  if (Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1) == '(') {
    Ahdrd_skipRead(ahdrd);    // skip '('
    ast = Ast_new();
    while (Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1) != ')') {
      ast = Ast_cons(Lex_parseS(), ast);
    }
    Ahdrd_skipRead(ahdrd);    // skip ')'
    return Ast_reverse(ast);
  }
  else
    return Lex_parseAtom();
}
