/*
  lexical analysis.
*/

#include <stdio.h>

#include "lex.h"
#include "ast.h"

char buf[2000];

char* lex_parse(FILE *fp) {
  int c;
  S *s = S_alloc();
  while ((c = fgetc(fp)) != EOF) {
  }
}

char* lex_getToken() {
  S *s;
  s = S_alloc();
}
