/*
  lexical analysis.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lex.h"
#include "ahdrd.h"
#include "prim.h"

static Ahdrd *ahdrd;

void Lex_init() {
  ahdrd = Ahdrd_new(stdin);
}

static S *Lex_parseAtom() {
  int c;
  if ((c = Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1)) == ':')
    return S_new(Keyword, Ahdrd_readKeyword(ahdrd));
  else if (c == '\'')
    return S_new(Character, Ahdrd_readCharacter(ahdrd));
  else if (c == '"')
    return S_new(String, Ahdrd_readString(ahdrd));
  else if (Ahdrd_isNumber(ahdrd))
    return S_new(Number, Ahdrd_readNumber(ahdrd));
  else {
    char* symbolName;
    return
      strcmp((symbolName = Ahdrd_readSymbol(ahdrd)), "t") == 0? t:
      strcmp(symbolName, "nil") == 0? nil:
      S_new(Symbol, symbolName);
  }
}

S *Lex_parseExpr() {
  S *expr;
  if (Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1) == '(') {
    expr = nil;
    Ahdrd_skipRead(ahdrd);    // skip '('
    while (Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1) != ')') {
      expr = cons(Lex_parseExpr(), expr);
    }
    Ahdrd_skipRead(ahdrd);    // skip ')'
    return reverse(expr);
  }
  else
    return Lex_parseAtom();
}
