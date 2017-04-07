/*
   ahead reader.
   */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <assert.h>

#include "ringbuf.h"

#define AHDRD_TOKEN_SIZE 2000

struct Ahdrd {
  FILE *fp;
  int tokenPos;
  char token[AHDRD_TOKEN_SIZE];
  struct Ringbuf ringbuf;
};

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
  if (!Ringbuf_isEmpty(&ahdrd->ringbuf))
    return Ringbuf_get(&ahdrd->ringbuf);
  if((c = fgetc(ahdrd->fp)) != EOF) {
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
  return ahdrd->ringbuf.buf[(ahdrd->ringbuf.in + n) % RINGBUF_BUFSIZ];
}

// assume input from stdin with '012345'
int main(void) {
  char c;
  char *str;
  struct Ahdrd ahdrd;
  Ahdrd_init(&ahdrd, stdin);
  assert(Ahdrd_read(&ahdrd) == '0');
  assert(Ahdrd_peek(&ahdrd, 1) == '1');
  assert(Ahdrd_peek(&ahdrd, 2) == '2');
  // printf("%c", Ahdrd_peek(&ahdrd, 2));
  // printf("%c", Ahdrd_peek(&ahdrd, 3));
  // while ((c = Ahdrd_read(&ahdrd)) != EOF)
  while ((c = Ahdrd_read(&ahdrd)) != EOF) {
    printf("%c", c);
  }
  return 0;
}
