/*
  ahead reader.
*/

#include "ringbuf.h"

#define AHDRD_TOKEN_SIZE 2000

typedef struct {
  FILE *fp;
  int tokenPos;
  char token[AHDRD_TOKEN_SIZE];
  Ringbuf ringbuf;
} Ahdrd;

extern void Ahdrd_init(Ahdrd *ahdrd, FILE *fp);
extern int Ahdrd_skipRead(Ahdrd *ahdrd);
extern int Ahdrd_read(Ahdrd *ahdrd);
extern int Ahdrd_peek(Ahdrd *ahdrd, int n);
extern int Ahdrd_peek1(Ahdrd *ahdrd);
extern char *Ahdrd_readChar(Ahdrd *ahdrd);
extern char *Ahdrd_readString(Ahdrd *ahdrd);
extern char *Ahdrd_readKeyword(Ahdrd *ahdrd);
extern char *Ahdrd_readSymbol(Ahdrd *ahdrd);
extern char *Ahdrd_readNumber(Ahdrd *ahdrd);
extern int Ahdrd_isNumber(Ahdrd *ahdrd);
