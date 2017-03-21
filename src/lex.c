/*
  lexical analysis.
*/

#include "lex.h"
#include "ringbuf.h"

static FILE *fp;
static char* buf;

char* lex_do(FILE *_fp) {
  fp = _fp;
  int c;
  while((c = getchar()) != EOF)
    ;
  return "c";
}
