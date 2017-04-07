/*
   ahead reader.
*/

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "ahdrd.h"

void Ahdrd_init(struct Ahdrd *ahdrd, FILE *_fp) {
  ahdrd->fp = _fp;
  ahdrd->tokenPos = 0;
  Ringbuf_init(&ahdrd->ringbuf);
}

char *Ahdrd_getToken(struct Ahdrd *ahdrd) {
  char *str;
  ahdrd->token[ahdrd->tokenPos] = '\0';
  if ((str = (char *)malloc(strlen(ahdrd->token) + 1)) == NULL) {
    fprintf(stderr, "Ahdrd_getToken: Cannot allocate memory.");
    exit(1);
  }
  ahdrd->tokenPos = 0;
  return strcpy(str, ahdrd->token);
}

int Ahdrd_read(struct Ahdrd *ahdrd) {
  int c;
  c = (!Ringbuf_isEmpty(&ahdrd->ringbuf))?
    Ringbuf_get(&ahdrd->ringbuf):
    fgetc(ahdrd->fp);
  if(c != EOF) {
    ahdrd->token[ahdrd->tokenPos] = c;
    ahdrd->tokenPos = ahdrd->tokenPos + 1;
  }
  return c;
}

int Ahdrd_peek(struct Ahdrd *ahdrd, int n) {
  int c;
  if (n <= 0) {
    fprintf(stderr, "Ahdrd_peek: Illegal argument.");
    exit(1);
  }
  if (n >= RINGBUF_BUFSIZ) {
    fprintf(stderr, "Ahdrd_peek: Buffer over flow.");
    exit(1);
  }
  while (n > Ringbuf_size(&ahdrd->ringbuf)) {
    if((c = fgetc(ahdrd->fp)) == EOF && n > Ringbuf_size(&ahdrd->ringbuf) + 1) {
      fprintf(stderr, "Ahdrd_peek: Reach EOF.");
      exit(1);
    }
    Ringbuf_put(&ahdrd->ringbuf, c);
  }
  return ahdrd->ringbuf.buf[(ahdrd->ringbuf.out + n - 1) % RINGBUF_BUFSIZ];
}
