/*
  paren reader.
*/

#include "lexcer.h"

typedef struct {
  Lexcer lex;
} Reader;

extern void Reader_init(Reader *rd, FILE *fp);
extern S *Reader_read(Reader *rd);
