/*
  lexical analysis.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lex.h"
#include "ahdrd.h"
#include "prim.h"

static struct Ahdrd *ahdrd;

void Lex_init() {
  ahdrd = Ahdrd_new(stdin);
}

S *Lex_parseAtom() {
  int c;
  S *atom;
  if ((c = Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1)) == ':')
    return S_newKeyword(Ahdrd_readKeyword(ahdrd));
  else if (c == '\'')
    return S_newCharacter(Ahdrd_readCharacter(ahdrd));
  else if (c == '"')
    return S_newString(Ahdrd_readString(ahdrd));
  else if (Ahdrd_isNumber(ahdrd))
    return S_newNumber(Ahdrd_readString(ahdrd));
  else
    return S_newSymbol(Ahdrd_readSymbol(ahdrd));
}

S *Lex_parseS() {
  S *expr;
  if (Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1) == '(') {
    Ahdrd_skipRead(ahdrd);    // skip '('
    while (Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1) != ')') {
      expr = cons(Lex_parseS(), expr);
    }
    Ahdrd_skipRead(ahdrd);    // skip ')'
    return reverse(expr);
  }
  else
    return Lex_parseAtom();
}
