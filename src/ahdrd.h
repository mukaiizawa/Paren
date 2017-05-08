/*
  ahead reader.
*/

#ifndef IS_LOADED_AHDRD
#define IS_LOADED_AHDRD

#include "ringbuf.h"

#define AHDRD_TOKEN_SIZE 2000

typedef struct {
  FILE *fp;
  int tokenPos;
  char token[AHDRD_TOKEN_SIZE];
  Ringbuf *ringbuf;
} Ahdrd;

extern Ahdrd *Ahdrd_new(FILE *_fp);
extern char *Ahdrd_getToken(Ahdrd *ahdrd);
extern int Ahdrd_skipRead(Ahdrd *ahdrd);
extern int Ahdrd_read(Ahdrd *ahdrd);
extern int Ahdrd_peek(Ahdrd *ahdrd, int n);
extern Ahdrd *Ahdrd_readSpace(Ahdrd *ahdrd);
extern char *Ahdrd_readCharacter(Ahdrd *ahdrd);
extern char *Ahdrd_readString(Ahdrd *ahdrd);
extern char *Ahdrd_readKeyword(Ahdrd *ahdrd);
extern char *Ahdrd_readSymbol(Ahdrd *ahdrd);
extern char *Ahdrd_readNumber(Ahdrd *ahdrd);
extern int Ahdrd_isNumber(Ahdrd *ahdrd);

#endif
