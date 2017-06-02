/*
  paren reader.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "splay.h"
#include "env.h"
#include "paren.h"
#include "reader.h"

void Reader_init(Reader *rd, FILE *fp) {
  Ahdrd_init(&rd->ahdrd, fp);
}

FILE *Reader_getFp(Reader *rd) {
  return rd->ahdrd.fp;
}

void Reader_setFp(Reader *rd, FILE *fp) {
  rd->ahdrd.fp = fp;
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

static S *Reader_eofError() {
  return Error_new("Error: reached eof.");
}

static S *Reader_parseKeyword(Reader *rd) {
  char *token;
  S *keyword;
  token = Ahdrd_readKeyword(&rd->ahdrd);
  if ((keyword = Env_getKeyword(&env, token)) != NULL) {
    free(token);
    return keyword;
  }
  return Keyword_new(token);
}

static S *Reader_parseChar(Reader *rd) {
  int n;
  char c, *token;
  if ((token = Ahdrd_readChar(&rd->ahdrd)) == NULL)
    return Reader_eofError();
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

static S *Reader_parseString(Reader *rd) {
  return String_new(Ahdrd_readString(&rd->ahdrd));
}

static S *Reader_parseNumber(Reader *rd) {
  char *token;
  double n;
  token = Ahdrd_readNumber(&rd->ahdrd);
  n = atof(token);
  free(token);
  return Number_new(n);
}

static S *Reader_parseSymbol(Reader *rd) {
  return Symbol_new(Ahdrd_readSymbol(&rd->ahdrd));
}

static S *Reader_parseAtom(Reader *rd) {
  int c;
  if ((c = Ahdrd_peek(Ahdrd_readSpace(&rd->ahdrd), 1)) == ':')
    return Reader_parseKeyword(rd);
  else if (c == '\'') 
    return Reader_parseChar(rd);
  else if (c == '"')
    return Reader_parseString(rd);
  else if (Ahdrd_isNumber(&rd->ahdrd))
    return Reader_parseNumber(rd);
  else
    return Reader_parseSymbol(rd);
}

static S *Reader_parseExpr(Reader *rd) {
  int c;
  S *acc, *expr;
  if ((c = Ahdrd_peek(Ahdrd_readSpace(&rd->ahdrd), 1)) == ')')
    return Reader_eofError();
  else if (c == '`') {
    Ahdrd_skipRead(&rd->ahdrd);    // skip '`'
    return Cons_new(quote, Cons_new(Reader_parseExpr(rd), nil));
  }
  else if (c == '(') {
    acc = nil;
    Ahdrd_skipRead(&rd->ahdrd);    // skip '('
    while (Ahdrd_peek(Ahdrd_readSpace(&rd->ahdrd), 1) != ')') {
      if (TYPEP((expr = Reader_parseExpr(rd)), Error))
        return expr;
      acc = Cons_new(expr, acc);
    }
    Ahdrd_skipRead(&rd->ahdrd);    // skip ')'
    return S_reverse(acc);
  }
  else
    return Reader_parseAtom(rd);
}

S *Reader_read(Reader *rd) {
  return Reader_parseExpr(rd);
}
