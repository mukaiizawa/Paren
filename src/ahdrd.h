/*
  ahead reader.
*/

#ifndef AHDRD
#define AHDRD

#include "ringbuf.h"

#define AHDRD_TOKEN_SIZE 2000

struct Ahdrd {
  FILE *fp;
  int tokenPos;
  char token[AHDRD_TOKEN_SIZE];
  struct Ringbuf *ringbuf;
};

extern struct Ahdrd *Ahdrd_new(FILE *_fp);
extern char *Ahdrd_getToken(struct Ahdrd *ahdrd);
extern int Ahdrd_skipRead(struct Ahdrd *ahdrd);
extern int Ahdrd_read(struct Ahdrd *ahdrd);
extern int Ahdrd_peek(struct Ahdrd *ahdrd, int n);
extern struct Ahdrd *Ahdrd_readSpace(struct Ahdrd *ahdrd);
extern char *Ahdrd_readCharacter(struct Ahdrd *ahdrd);
extern char *Ahdrd_readString(struct Ahdrd *ahdrd);
extern char *Ahdrd_readKeyword(struct Ahdrd *ahdrd);
extern char *Ahdrd_readSymbol(struct Ahdrd *ahdrd);
extern char *Ahdrd_readNumber(struct Ahdrd *ahdrd);
extern int Ahdrd_isNumber(struct Ahdrd *ahdrd);

#endif
