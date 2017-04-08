/*
  lexical analysis.
*/

#include <stdio.h>

#include "lex.h"
#include "ast.h"
#include "ahdrd.h"

static char buf[10];
static int ch;
static int pos;

static struct Ahdrd ahdrd;

void Lex_init() {
  Ahdrd_init(&ahdrd, stdin);
}

struct S *Lex_parse() {
  Lex_init();
  return Lex_parseS();
}

struct S *Lex_nextToken() {
  struct S *s;
  s = S_alloc();
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

struct S *Lex_parseS() {
  struct S *prev, *car, *cdr;
  if (Ahdrd_peek(&ahdrd, 1) != '(')
    return S_consNil(Lex_nextToken());
  else {
    Ahdrd_skipRead(&ahdrd);
    while (Ahdrd_peek(&ahdrd, 1) == ')') {
      car = Lex_parseS();
      if (Ahdrd_peek(&ahdrd, 1) == ')')
        prev = S_consNil(car);
      else {
        cdr = Lex_parseS();
        prev = S_cons(car, cdr);
      }
    }
    Ahdrd_skipRead(&ahdrd);
  }
  return prev;
}
