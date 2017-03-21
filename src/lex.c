/*
  lexical analysis.
*/

#include "lex.h"

char* lex_do(FILE *fp) {
  int c;
  while((c = getchar()) != '\n')
    ;
  return "c";
}
