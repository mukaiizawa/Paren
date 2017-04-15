/*
   ahead reader.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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

int Ahdrd_skipRead(struct Ahdrd *ahdrd) {
  return (!Ringbuf_isEmpty(&ahdrd->ringbuf))?
    Ringbuf_get(&ahdrd->ringbuf):
    fgetc(ahdrd->fp);
}

int Ahdrd_read(struct Ahdrd *ahdrd) {
  int c;
  if((c = Ahdrd_skipRead(ahdrd)) != EOF) {
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

struct Ahdrd *Ahdrd_readSpace(struct Ahdrd *ahdrd) {
  int c;
  while ((c = Ahdrd_peek(ahdrd, 1)) != EOF && isspace(c))
    Ahdrd_skipRead(ahdrd);
  return ahdrd;
}

char* Ahdrd_readKeyword(struct Ahdrd *ahdrd) {
  int c;
  Ahdrd_skipRead(ahdrd);    // skip ':'
  while (!isspace((c = Ahdrd_peek(ahdrd, 1))) && c != '(' && c != ')')
    Ahdrd_read(ahdrd);
  return Ahdrd_getToken(ahdrd);
}

char *Ahdrd_readCharacter(struct Ahdrd *ahdrd) {
  int c, readCount;
  Ahdrd_skipRead(ahdrd);    // skip `'`
  readCount = 0;
  while ((c = Ahdrd_peek(ahdrd, 1)) != '\'') {
    Ahdrd_read(ahdrd);
    if (++readCount > 1) {
      fprintf(stderr, "Ahdrd_readCharacter: Illegal token.");
      exit(1);
    }
    if (c == '\'')
      Ahdrd_read(ahdrd);
  }
  Ahdrd_skipRead(ahdrd);    // skip `'`
  return Ahdrd_getToken(ahdrd);
}

char* Ahdrd_readString(struct Ahdrd *ahdrd) {
  int c;
  Ahdrd_skipRead(ahdrd);    // skip `"`
  while ((c = Ahdrd_peek(ahdrd, 1)) != '"') {
    Ahdrd_read(ahdrd);
    if (c == '\\')
      Ahdrd_read(ahdrd);
  }
  Ahdrd_skipRead(ahdrd);    // skip `"`
  return Ahdrd_getToken(ahdrd);
}

char* Ahdrd_readSymbol(struct Ahdrd *ahdrd) {
  int c;
  while (!isspace((c = Ahdrd_peek(ahdrd, 1))) && c != '(' && c != ')')
    Ahdrd_read(ahdrd);
  return Ahdrd_getToken(ahdrd);
}

int Ahdrd_isNumber(struct Ahdrd *ahdrd) {
  int i, c;
  i = 1;
  if (!isdigit((c = Ahdrd_peek(ahdrd, 1))) || c == '0')
    return 0;
  while (!isspace((c = Ahdrd_peek(ahdrd, ++i)))
      && c != '(' && c != ')' && c != '.') {
    if (!isdigit(c))
      return 0;
  }
  if (c == '.') {
    while (!isspace((c = Ahdrd_peek(ahdrd, ++i)))
        && c != '(' && c != ')') {
      if (!isdigit(c))
        return 0;
    }
  }
  return 1;
}
