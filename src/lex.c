/*
  lexical analysis.
*/

#include "lex.h"
#include "ahdrd.h"

static Ahdrd ahdrd;

char* lex_start(FILE *fp) {
  int c;
  Ahdrd ahdrd;
  ahdrd_init(&ahdrd, fp);
  while((c = ahdrd_getch(&ahdrd)) != EOF)
    fputc(c, stdout);
  return "c";
}
