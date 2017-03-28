/*
  lexical analysis.
*/

#include "lex.h"
#include "ahdrd.h"

static Ahdrd ahdrd;

void lex_init(FILE *fp) {
  ahdrd_init(&ahdrd, fp);
}

char* lex_start(FILE *fp) {
  int c;
  lex_init(fp);
  while((c = ahdrd_getch(&ahdrd)) != EOF)
    fputc(c, stdout);
  return "c";
}
