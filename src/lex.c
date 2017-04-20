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

struct S *Lex_parseAtom() {
  int c;
  struct S *atom;
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

struct S *Lex_parseS() {
  struct S *s;
  if (Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1) == '(') {
    Ahdrd_skipRead(ahdrd);    // skip '('
    s = S_new();
    while (Ahdrd_peek(Ahdrd_readSpace(ahdrd), 1) != ')') {
      s = S_cons(Lex_parseS(), s);
    }
    Ahdrd_skipRead(ahdrd);    // skip ')'
    return S_reverse(s);
  }
  else
    return Lex_parseAtom();
}
