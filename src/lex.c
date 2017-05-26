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
static Env *env;

void Lex_init(Env *_env) {
  Ahdrd_init(&ahdrd, stdin);
  env = _env;
}

static S *S_reverse(S *expr) {
  S *root;
  if (NILP(expr))
    return nil;
  assert(TYPEP(expr, Cons));
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
  char *token;
  S *keyword;
  token = Ahdrd_readKeyword(&ahdrd);
  if ((keyword = Env_getKeyword(env, token)) != NULL) {
    free(token);
    return keyword;
  }
  return Keyword_new(token);
}

static S *Lex_parseChar() {
  int n;
  char c, *token;
  if ((token = Ahdrd_readChar(&ahdrd)) == NULL)
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
  return Char_new(c);
}

static S *Lex_parseString() {
  return String_new(Ahdrd_readString(&ahdrd));
}

static S *Lex_parseNumber() {
  char *token;
  double n;
  token = Ahdrd_readNumber(&ahdrd);
  n = atof(token);
  free(token);
  return Number_new(n);
}

static S *Lex_parseSymbol() {
  return Symbol_new(Ahdrd_readSymbol(&ahdrd));
}

static S *Lex_parseAtom() {
  int c;
  if ((c = Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1)) == ':')
    return Lex_parseKeyword();
  else if (c == '\'') 
    return Lex_parseChar();
  else if (c == '"')
    return Lex_parseString();
  else if (Ahdrd_isNumber(&ahdrd))
    return Lex_parseNumber();
  else
    return Lex_parseSymbol();
}

S *Lex_parseExpr() {
  int c;
  S *acc, *expr;
  if ((c = Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1)) == ')')
    return Lex_eofError();
  else if (c == '(') {
    acc = nil;
    Ahdrd_skipRead(&ahdrd);    // skip '('
    while (Ahdrd_peek(Ahdrd_readSpace(&ahdrd), 1) != ')') {
      if (TYPEP((expr = Lex_parseExpr()), Error))
        return expr;
      acc = Cons_new(expr, acc);
    }
    Ahdrd_skipRead(&ahdrd);    // skip ')'
    return S_reverse(acc);
  }
  else
    return Lex_parseAtom();
}
