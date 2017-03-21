/*
  lexical analysis.
*/

#include "lex.h"
#include "ringbuf.h"

static FILE *fp;
static struct ringbuf rb;

void lex_init(FILE *_fp) {
  fp = _fp;
  ringbuf_reset(&rb);
}

char* lex_start(FILE *_fp) {
  lex_init(_fp);
  int c;
  while((c = fgetc(fp)) != EOF)
    fputc(c, stdout);
  return "c";
}
