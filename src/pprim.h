/*
  paren primitive.
*/

#ifndef PPRIM
#define PPRIM

#define AHDRD_TOKEN_SIZE 2000

struct Ahdrd {
  FILE *fp;
  int tokenPos;
  char token[AHDRD_TOKEN_SIZE];
  struct Ringbuf ringbuf;
};

extern void Ahdrd_init(struct Ahdrd *ahdrd, FILE *_fp);
extern char *Ahdrd_getToken(struct Ahdrd *ahdrd);
extern int Ahdrd_skipRead(struct Ahdrd *ahdrd);
extern int Ahdrd_read(struct Ahdrd *ahdrd);
extern int Ahdrd_peek(struct Ahdrd *ahdrd, int n);
extern char *Ahdrd_readKeyword(struct Ahdrd *ahdrd);

#endif
