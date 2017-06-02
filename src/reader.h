/*
  paren reader.
*/

#include "ahdrd.h"

typedef struct {
  Ahdrd ahdrd;
} Reader;

extern void Reader_init(Reader *rd, FILE *fp);
extern S *Reader_read(Reader *rd);
extern FILE *Reader_getFp(Reader *rd);
extern void Reader_setFp(Reader *rd, FILE *fp);
