/*
  lexical analysis.
*/

#include <stdio.h>

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
  Lex_init();
  return Lex_parseS();
}

struct Ast *Lex_parseAtom() {
  struct Ast *s;
  s = Ast_alloc();
  int next;
  while ((next = Ahdrd_peek(&ahdrd, 1)) != '\n') {
    switch (next) {
      case ':':
        s->type = KEYWORD;
        s->val = Ahdrd_readKeyword(&ahdrd);
        return s;
      case '\'':
        return s;
    }
  }
}

struct Ast *Lex_parseS() {
  struct Ast *prev, *car, *cdr;
  if (Ahdrd_peek(&ahdrd, 1) != '(')
    return Ast_consWithNil(Lex_parseAtom());
  else
    // {
    //   Ahdrd_skipRead(&ahdrd);
    //   while (Ahdrd_peek(&ahdrd, 1) == ')') {
    //     car = Lex_parseS();
    //     if (Ahdrd_peek(&ahdrd, 1) == ')')
    //       prev = Ast_consWithNil(car);
    //     else {
    //       cdr = Lex_parseS();
    //       prev = Ast_cons(car, cdr);
    //     }
    //   }
    //   Ahdrd_skipRead(&ahdrd);
    // }
    return prev;
}
