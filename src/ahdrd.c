/*
   ahead reader.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "std.h"
#include "ahdrd.h"

void Ahdrd_init(Ahdrd *ahdrd, FILE *fp) {
  ahdrd->fp = fp;
  ahdrd->tokenPos = 0;
  Ringbuf_init(&ahdrd->ringbuf);
}

static char *Ahdrd_getToken(Ahdrd *ahdrd) {
  char *str;
  ahdrd->token[ahdrd->tokenPos] = '\0';
  str = xmalloc(strlen(ahdrd->token) + 1);
  ahdrd->tokenPos = 0;
  return strcpy(str, ahdrd->token);
}

static void Ahdrd_eofError(char *msg) {
  fprintf(stderr, "read: %s.\n", msg);
  exit(1);
}

static int Ahdrd_isToken(char c) {
  return !isspace(c) && c != '(' && c != ')' && c != EOF;
}

int Ahdrd_skipRead(Ahdrd *ahdrd) {
  return (!Ringbuf_isEmpty(&ahdrd->ringbuf))?
    Ringbuf_get(&ahdrd->ringbuf):
    fgetc(ahdrd->fp);
}

int Ahdrd_read(Ahdrd *ahdrd) {
  int c;
  if((c = Ahdrd_skipRead(ahdrd)) != EOF) {
    ahdrd->token[ahdrd->tokenPos] = c;
    ahdrd->tokenPos = ahdrd->tokenPos + 1;
  }
  return c;
}

int Ahdrd_peek(Ahdrd *ahdrd, int n) {
  int c;
  assert(0 < n && n < RINGBUF_BUFSIZ);
  while (n > Ringbuf_size(&ahdrd->ringbuf)) {
    c = fgetc(ahdrd->fp);
    Ringbuf_put(&ahdrd->ringbuf, c);
    if (c == EOF && n > Ringbuf_size(&ahdrd->ringbuf)) {
      fprintf(stderr, "ahdrd: reached eof\n");
      exit(1);
    }
  }
  return ahdrd->ringbuf.buf[(ahdrd->ringbuf.out + n - 1) % RINGBUF_BUFSIZ];
}

int Ahdrd_peek1(Ahdrd *ahdrd) {
  return Ahdrd_peek(ahdrd, 1);
}

static char *Ahdrd_readSurrounded(Ahdrd *ahdrd, char s) {
  int c;
  Ahdrd_skipRead(ahdrd);    // skip surround start
  while ((c = Ahdrd_peek1(ahdrd)) != s) {
    if (c == EOF) Ahdrd_eofError("quote not closed");
    Ahdrd_read(ahdrd);
    if (c == '\\') Ahdrd_read(ahdrd);
  }
  Ahdrd_skipRead(ahdrd);    // skip surround end
  return Ahdrd_getToken(ahdrd);
}

char *Ahdrd_readChar(Ahdrd *ahdrd) {
  return Ahdrd_readSurrounded(ahdrd, '\'');
}

char *Ahdrd_readString(Ahdrd *ahdrd) {
  return Ahdrd_readSurrounded(ahdrd, '"');
}

char *Ahdrd_readSymbol(Ahdrd *ahdrd) {
  while (Ahdrd_isToken(Ahdrd_peek1(ahdrd))) Ahdrd_read(ahdrd);
  return Ahdrd_getToken(ahdrd);
}

char *Ahdrd_readKeyword(Ahdrd *ahdrd) {
  Ahdrd_skipRead(ahdrd);    // skip ':'
  return Ahdrd_readSymbol(ahdrd);
}

char *Ahdrd_readNumber(Ahdrd *ahdrd) {
  return Ahdrd_readSymbol(ahdrd);
}

int Ahdrd_isNumber(Ahdrd *ahdrd) {
  int i, c;
  i = 1;
  if ((c = Ahdrd_peek(ahdrd, i)) == '+' || c == '-')
    if (!Ahdrd_isToken(Ahdrd_peek(ahdrd, ++i))) return 0;
  while (isdigit(c = Ahdrd_peek(ahdrd, i++)));
  if (!Ahdrd_isToken(c)) return 1;
  if (c != '.') return 0;
  while (isdigit(c = Ahdrd_peek(ahdrd, ++i)));
  return !Ahdrd_isToken(c);
}
