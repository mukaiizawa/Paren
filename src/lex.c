/*
  lexical analysis.
*/

#include <stdio.h>

#include "lex.h"
#include "ast.h"

static char buf[10];
static FILE *fp;
static int ch;
static int pos;

void lex_init() {
  fp = stdin;
  pos = 0;
}

struct S *lex_parse() {
  struct S root;
  lex_init();
  while ((c = fgetc(fp)) != EOF) {
  }
  return root;
}

void lex_getCh() {
  fp = stdin;
}

void lex_seek(int n) {
  fp = stdin;
}

void lex_readSpace() {
}

struct S *lex_readS() {
}

struct S *lex_readList() {
}

struct S *lex_readAtom() {
}

struct S *lex_readSymbol() {
}

struct S *lex_readKeyword() {
}

struct S *lex_readNumber() {
}

struct S *lex_readChar() {
}

struct S *lex_readString() {
}
