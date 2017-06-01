/*
  paren reader.
*/

#include <stdio.h>

#include "splay.h"
#include "env.h"
#include "paren.h"
#include "reader.h"

void Reader_init(Reader *reader, Env *env, FILE *fp) {
  Lexcer_init(&reader->lexcer, env, fp);
}

S *Reader_read(Reader *reader) {
  return Lexcer_parseExpr(&reader->lexcer);
}
