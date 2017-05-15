/*
  lexical analysis.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "splay.h"
#include "env.h"
#include "prim.h"
#include "lex.h"

static Ahdrd ahdrd;

void Lex_init() {
  Ahdrd_init(&ahdrd, stdin);
}

static S *S_reverse(S *expr) {
  S *root;
  if (NILP(expr))
    return nil;
  assert(expr->Cons.type == Cons);
  root = nil;
  while (!NILP(expr)) {
    root = Cons_new(FIRST(expr), root);
    expr = REST(expr);
  }
  return root;
}

// TODO: free memory.
static S *Lex_parseAtom() {
  int c;
  char *token;
  if ((c = Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1)) == ':')
    return Keyword_new(Ahdrd_readKeyword(&ahdrd));
  else if (c == '\'')
    return Character_new(Ahdrd_readCharacter(&ahdrd)[0]);
  else if (c == '"')
    return String_new(Ahdrd_readString(&ahdrd));
  else if (Ahdrd_isNumber(&ahdrd))
    return Number_new(atof(Ahdrd_readNumber(&ahdrd)));
  else
    return
      strcmp((token = Ahdrd_readSymbol(&ahdrd)), "t") == 0? t:
      strcmp(token, "nil") == 0? nil:
      Symbol_new(token);
}

S *Lex_parseExpr() {
  S *expr;
  if (Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1) == '(') {
    expr = nil;
    Ahdrd_skipRead(&ahdrd);    // skip '('
    while (Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1) != ')') {
      expr = Cons_new(Lex_parseExpr(), expr);
    }
    Ahdrd_skipRead(&ahdrd);    // skip ')'
    return S_reverse(expr);
  }
  return Lex_parseAtom();
}
