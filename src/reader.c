/*
  paren reader.
*/

#include <stdio.h>

#include "splay.h"
#include "env.h"
#include "paren.h"
#include "reader.h"

void Reader_init(Reader *rd, FILE *fp) {
  Lexcer_init(&rd->lex, fp);
}

S *Reader_read(Reader *rd) {
  return Lexcer_parseExpr(&rd->lex);
}
