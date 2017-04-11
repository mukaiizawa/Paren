/*
  lexical analysis.
*/

#include <stdio.h>
#include <stdlib.h>

#include "lex.h"
#include "ast.h"
#include "ahdrd.h"
#include "pprim.h"

static struct Ahdrd ahdrd;

void Lex_init() {
  Ahdrd_init(&ahdrd, stdin);
}

struct Ast *Lex_parse() {
  return Lex_parseS();
}

struct Ast *Lex_parseAtom() {
  struct Ast *atom;
  atom = Ast_alloc();
  switch (Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1)) {
    case ':':
      atom->type = KEYWORD;
      atom->val = Ahdrd_readKeyword(&ahdrd);
      break;
    case '\'':
      atom->type = CHARACTER;
      atom->val = Ahdrd_readCharacter(&ahdrd);
      break;
    case '"':
      atom->type = STRING;
      atom->val = Ahdrd_readString(&ahdrd);
      break;
    default:
      fprintf(stderr, "Lex_parseAtom: Illegal token.");
      exit(1);
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
