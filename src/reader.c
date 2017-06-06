/*
  paren reader.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <math.h>

#include "splay.h"
#include "env.h"
#include "paren.h"
#include "reader.h"

void Reader_init(Reader *rd, FILE *fp, S *eof) {
  rd->eof = eof;
  Ahdrd_init(&rd->ahdrd, fp);
}

static S *Reader_error(Reader *rd, char *msg) {
  // fprintf(stderr, "read: %s.\n", msg);
  while (Ahdrd_peek1(&rd->ahdrd) != EOF) Ahdrd_skipRead(&rd->ahdrd);
  Ahdrd_skipRead(&rd->ahdrd);
  return Error_new(msg);
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

static S *Reader_parseSpace(Reader *rd) {
  int c;
  if ((c = Ahdrd_peek1(&rd->ahdrd)) == EOF) return rd->eof;
  else if (isspace(c)) {
    while ((c = Ahdrd_peek1(&rd->ahdrd)) != EOF && isspace(c))
      Ahdrd_skipRead(&rd->ahdrd);
    return Reader_parseSpace(rd);
  }
  else if (c == ';') {
    while ((c = Ahdrd_peek1(&rd->ahdrd)) != EOF && c != '\n')
      Ahdrd_skipRead(&rd->ahdrd);
    return Reader_parseSpace(rd);
  }
  else if (c == '#' && Ahdrd_peek(&rd->ahdrd, 2) == '|') {
    while ((c = Ahdrd_peek1(&rd->ahdrd)) != '|'
        || Ahdrd_peek(&rd->ahdrd, 2) != '#') {
      if (c == EOF) return Reader_error(rd, "comment #| |# not closed");
      Ahdrd_skipRead(&rd->ahdrd);
    }
    Ahdrd_skipRead(&rd->ahdrd);    // skip '|'
    Ahdrd_skipRead(&rd->ahdrd);    // skip '#'
    return Reader_parseSpace(rd);
  }
  return NULL;
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
    return Reader_error(rd, "missing `'`");
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
  if ((c = Ahdrd_peek1(&rd->ahdrd)) == ':') return Reader_parseKeyword(rd);
  else if (c == '\'') return Reader_parseChar(rd);
  else if (c == '"') return Reader_parseString(rd);
  else if (Ahdrd_isNumber(&rd->ahdrd)) return Reader_parseNumber(rd);
  else return Reader_parseSymbol(rd);
}

static S *Reader_parseExpr(Reader *rd) {
  int c;
  S *acc, *expr;
  if ((expr = Reader_parseSpace(rd)) != NULL) return expr;
  else if ((c = Ahdrd_peek1(&rd->ahdrd)) == '`') {
    Ahdrd_skipRead(&rd->ahdrd);    // skip '`'
    return Cons_new(quote, Cons_new(Reader_parseExpr(rd), nil));
  }
  else if (c == ')') return Reader_error(rd, "unexpected token ')'");
  else if (c == '(') {
    acc = nil;
    Ahdrd_skipRead(&rd->ahdrd);    // skip '('
    while ((c = Ahdrd_peek1(&rd->ahdrd)) != ')') {
      if (c == EOF) return Reader_error(rd, "missing ')'");
      if (TYPEP((expr = Reader_parseExpr(rd)), Error)) return expr;
      acc = Cons_new(expr, acc);
    }
    Ahdrd_skipRead(&rd->ahdrd);    // skip ')'
    return S_reverse(acc);
  }
  else return Reader_parseAtom(rd);
}

S *Reader_read(Reader *rd) {
  return Reader_parseExpr(rd);
}
