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
  assert(S_isType(expr, Cons));
  root = nil;
  while (!NILP(expr)) {
    root = Cons_new(FIRST(expr), root);
    expr = REST(expr);
  }
  return root;
}

static S *Lex_eofError() {
  return Error_new("Error: reached eof.");
}

static S *Lex_parseKeyword() {
  return Keyword_new(Ahdrd_readKeyword(&ahdrd));
}

static S *Lex_parseCharacter() {
  int n;
  char c, *token;
  if ((token = Ahdrd_readCharacter(&ahdrd)) == NULL)
    return Lex_eofError();
  if ((n = strlen(token)) == 1)
    c = token[0];
  else if (n == 2 && token[0] == '\\') {
    switch (token[1]) {
      case 'n': c = '\n'; break;
      case 't': c = '\t'; break;
      default: c = token[1];
    }
  }
  else
    c = '\0';
  free(token);
  return Character_new(c);
}

static S *Lex_parseString() {
  return String_new(Ahdrd_readString(&ahdrd));
}

static S *Lex_parseNumber() {
  return Number_new(atof(Ahdrd_readNumber(&ahdrd)));
}

static S *Lex_parseSymbol() {
  char *token;
  return
    strcmp((token = Ahdrd_readSymbol(&ahdrd)), "t") == 0? t:
    strcmp(token, "nil") == 0? nil:
    Symbol_new(token);
}

static S *Lex_parseAtom() {
  int c;
  if ((c = Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1)) == ':')
    return Lex_parseKeyword();
  else if (c == '\'') 
    return Lex_parseCharacter();
  else if (c == '"')
    return Lex_parseString();
  else if (Ahdrd_isNumber(&ahdrd))
    return Lex_parseNumber();
  else
    return Lex_parseSymbol();
}

S *Lex_parseExpr() {
  S *acc, *expr;
  if (Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1) == '(') {
    acc = nil;
    Ahdrd_skipRead(&ahdrd);    // skip '('
    while (Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1) != ')') {
      if (S_isType((expr = Lex_parseExpr()), Error))
        return expr;
      acc = Cons_new(expr, acc);
    }
    Ahdrd_skipRead(&ahdrd);    // skip ')'
    return S_reverse(acc);
  }
  return Lex_parseAtom();
}
