/*
  paren reader.
*/

#include "lexcer.h"

typedef struct {
  Lexcer lexcer;
  FILE *fp;
} Reader;

extern void Reader_init(Reader *reader, Env *env, FILE *fp);
extern S *Reader_read();
