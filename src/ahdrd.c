/*
   ahead reader.
*/

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

// #include "ahdrd.h"

#define AHDRD_TOKEN_SIZE 2000
#define RINGBUF_BUF_SIZE 4

struct Ahdrd {
  FILE *fp;
  int tokenPos;
  char token[AHDRD_TOKEN_SIZE];
  struct Ringbuf {
    int in;
    int out;
    int buf[RINGBUF_BUF_SIZE];
  } ringbuf;
};

void Ahdrd_init(struct Ahdrd *ahdrd, FILE *_fp) {
  ahdrd->fp = _fp;
  ahdrd->tokenPos = 0;
  ahdrd->ringbuf.in = 0;
  ahdrd->ringbuf.out = 0;
}

char *Ahdrd_getToken(struct Ahdrd *ahdrd) {
  char *str;
  if ((str = (char *)malloc(strlen(ahdrd->token) + 1)) == NULL) {
    fprintf(stderr, "Ahdrd_getToken: Cannot allocate memory.");
    exit(1);
  }
  return strcpy(str, ahdrd->token);
}

int Ahdrd_isEmptyBuf(struct Ahdrd *ahdrd) {
  return ahdrd->ringbuf.in == ahdrd->ringbuf.out;
}

void Ahdrd_putBuf(struct Ahdrd *ahdrd, int c) {
  int nextIn;
  nextIn = (ahdrd->ringbuf.in + 1) % RINGBUF_BUF_SIZE;
  if (nextIn == ahdrd->ringbuf.out) {
    fprintf(stderr, "Ahdrd_putBuf: Buffer over flow.");
    exit(1);
  }
  ahdrd->ringbuf.buf[ahdrd->ringbuf.in] = c;
  ahdrd->ringbuf.in = nextIn;
}

int Ahdrd_getBuf(struct Ahdrd *ahdrd) {
  int c;
  if (Ahdrd_isEmptyBuf(ahdrd)) {
    fprintf(stderr, "Ahdrd_getBuf: Buffer empty.");
    exit(1);
  }
  c = ahdrd->ringbuf.buf[ahdrd->ringbuf.out];
  ahdrd->ringbuf.out = (ahdrd->ringbuf.out + 1) % RINGBUF_BUF_SIZE;
  return c;
}

int Ahdrd_read(struct Ahdrd *ahdrd) {
  int c;
  if (!Ahdrd_isEmptyBuf(ahdrd))
    return Ahdrd_getBuf(ahdrd);
  if((c = fgetc(ahdrd->fp)) != EOF) {
    ahdrd->token[ahdrd->tokenPos] = c;
    ahdrd->tokenPos = ahdrd->tokenPos + 1;
    Ahdrd_putBuf(ahdrd, c);
  }
  return c;
}

int Ahdrd_peek(struct Ahdrd *ahdrd, int n) {
  int c;
  if (n <= 0) {
    fprintf(stderr, "Ahdrd_peek: Illegal argument.");
    exit(1);
  }
  if (n >= RINGBUF_BUF_SIZE) {
    fprintf(stderr, "Ahdrd_peek: Buffer over flow.");
    exit(1);
  }
  if (n > ahdrd->tokenPos)
    while ((ahdrd->tokenPos = ahdrd->tokenPos + 1) < n) {
      if((c = fgetc(ahdrd->fp)) == EOF && ahdrd->tokenPos < n ) {
        fprintf(stderr, "Ahdrd_peek: Reach EOF.");
        exit(1);
      }
      ahdrd->ringbuf.buf[ahdrd->tokenPos] = c;
    }
  return ahdrd->ringbuf.buf[n - 1];
}

// char Ahdrd_peek(struct Ahdrd *ahdrd, int n) {
//   if (ahdrd->pos == 0) {
//   return ahdrd->c;
// }

int main(void) {
  char *str;
  struct Ahdrd ahdrd;
  Ahdrd_init(&ahdrd, stdin);
  Ahdrd_putBuf(&ahdrd, '1');
  Ahdrd_putBuf(&ahdrd, '2');
  Ahdrd_putBuf(&ahdrd, '3');
  // str = Ahdrd_getToken(&ahdrd);
  while (!Ahdrd_isEmptyBuf(&ahdrd)) {
    printf("%c", Ahdrd_getBuf(&ahdrd));
  }
  printf("%c", Ahdrd_peek(&ahdrd, 3));
  // while (Ahdrd_getBuf(&ahdrd) != EOF) {
  //   printf("%c", Ahdrd_read(&ahdrd));
  // }
  return 0;
}
