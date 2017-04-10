/*
  lexical analysis.
*/

#include <stdio.h>
#include <stdlib.h>

#include "lex.h"
#include "ast.h"
#include "ahdrd.h"
#include "pprim.h"

static char buf[10];
static int ch;
static int pos;

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
  int next;
  Ahdrd_readSpace(&ahdrd);
  switch (Ahdrd_peek(&ahdrd, 1)) {
    case ':':
      atom->type = KEYWORD;
      atom->val = Ahdrd_readKeyword(&ahdrd);
      return atom;
    case '\'':
      atom->type = CHARACTER;
      atom->val = Ahdrd_readCharacter(&ahdrd);
      return atom;
  }
  fprintf(stderr, "Lex_parseAtom: Illegal token.");
  exit(1);
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
    return ast;
  }
  else
    return Lex_parseAtom();
}
