/*
  paren reader.
*/

#include "ahdrd.h"

typedef struct {
  Ahdrd ahdrd;
  S *eof;
} Reader;

extern void Reader_init(Reader *rd, FILE *fp, S *eof);
extern S *Reader_read(Reader *rd);
