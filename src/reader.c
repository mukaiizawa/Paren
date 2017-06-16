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

static int Reader_nextChar(Reader *rd) {
  int c;
  while ((c = Ahdrd_peek1(&rd->ahdrd)) != EOF && isspace(c))
    Ahdrd_skipRead(&rd->ahdrd);
  return c;
}

static S *Reader_parseKeyword(Reader *rd) {
  return Keyword_new(Ahdrd_readKeyword(&rd->ahdrd));
}

static S *Reader_parseChar(Reader *rd) {
  int n;
  char c, *token;
  if ((n = strlen(token = Ahdrd_readChar(&rd->ahdrd))) == 1) c = token[0];
  else if (n == 2 && token[0] == '\\') {
    switch (token[1]) {
      case 'n': c = '\n'; break;
      case 't': c = '\t'; break;
      default: c = token[1];
    }
  }
  else c = '\0';
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

extern S *Reader_parseAtom(Reader *rd);

static S *Reader_parseLineComment(Reader *rd) {
  int c;
  while ((c = Ahdrd_peek1(&rd->ahdrd)) != EOF && c != '\n')
    Ahdrd_skipRead(&rd->ahdrd);
  return Reader_parseAtom(rd);
}

static S *Reader_parseComment(Reader *rd) {
  int c;
  while ((c = Ahdrd_peek1(&rd->ahdrd)) != '|'
      || Ahdrd_peek(&rd->ahdrd, 2) != '#') {
    if (c == EOF) return Error_msg("comment #| |# not closed");
    Ahdrd_skipRead(&rd->ahdrd);
  }
  Ahdrd_skipRead(&rd->ahdrd);    // skip '|'
  Ahdrd_skipRead(&rd->ahdrd);    // skip '#'
  return Reader_parseAtom(rd);
}

S *Reader_parseAtom(Reader *rd) {
  int c;
  if ((c = Reader_nextChar(rd)) == ':') return Reader_parseKeyword(rd);
  else if (c == '\'') return Reader_parseChar(rd);
  else if (c == '"') return Reader_parseString(rd);
  else if (c == ';') return Reader_parseLineComment(rd);
  else if (c == '#' && Ahdrd_peek(&rd->ahdrd, 2) == '|')
    return Reader_parseComment(rd);
  else if (Ahdrd_isNumber(&rd->ahdrd)) return Reader_parseNumber(rd);
  else return Reader_parseSymbol(rd);
}

static S *Reader_parseExpr(Reader *rd) {
  int c;
  if ((c = Reader_nextChar(rd)) == EOF) return rd->eof;
  else if (c == '`') {
    Ahdrd_skipRead(&rd->ahdrd);    // skip '`'
    return Cons_new(quote, Cons_new(Reader_parseExpr(rd), nil));
  }
  else if (c == '(') {
    S *acc, *expr;
    acc = nil;
    Ahdrd_skipRead(&rd->ahdrd);    // skip '('
    while ((c = Reader_nextChar(rd)) != ')') {
      if (c == EOF) return Error_msg("missing ')'");
      if (TYPEP((expr = Reader_parseExpr(rd)), Error)) return expr;
      acc = Cons_new(expr, acc);
    }
    Ahdrd_skipRead(&rd->ahdrd);    // skip ')'
    return REVERSE(acc);
  }
  else if (c != ')') return Reader_parseAtom(rd);
  else {
    while (Ahdrd_peek1(&rd->ahdrd) == ')') Ahdrd_skipRead(&rd->ahdrd);
    return Error_msg("unexpected token ')'");
  }
}

S *Reader_read(Reader *rd) {
  return Reader_parseExpr(rd);
}
