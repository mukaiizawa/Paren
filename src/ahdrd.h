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
  struct Ringbuf ringbuf;
};

void Ahdrd_init(struct Ahdrd *ahdrd, FILE *_fp);
char *Ahdrd_getToken(struct Ahdrd *ahdrd);
int Ahdrd_read(struct Ahdrd *ahdrd);
int Ahdrd_peek(struct Ahdrd *ahdrd, int n);

#endif
