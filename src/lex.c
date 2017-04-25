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
    return S_newExpr(Keyword, Ahdrd_readKeyword(ahdrd));
  else if (c == '\'')
    return S_newExpr(Character, Ahdrd_readCharacter(ahdrd));
  else if (c == '"')
    return S_newExpr(String, Ahdrd_readString(ahdrd));
  else if (Ahdrd_isNumber(ahdrd))
    return S_newExpr(Number, Ahdrd_readNumber(ahdrd));
  else
    return S_newExpr(Symbol, Ahdrd_readSymbol(ahdrd));
}

S *Lex_parseExpr() {
  S *expr;
  if (Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1) == '(') {
    expr = S_newNil();
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
