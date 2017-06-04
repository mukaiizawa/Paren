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
  assert(str != NULL);
  ahdrd->tokenPos = 0;
  return strcpy(str, ahdrd->token);
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
    assert(c != EOF || n <= Ringbuf_size(&ahdrd->ringbuf) + 1);
    Ringbuf_put(&ahdrd->ringbuf, c);
  }
  return ahdrd->ringbuf.buf[(ahdrd->ringbuf.out + n - 1) % RINGBUF_BUFSIZ];
}

Ahdrd *Ahdrd_readSpace(Ahdrd *ahdrd) {
  int c;
  if ((c = Ahdrd_peek(ahdrd, 1)) == EOF)
    return ahdrd;
  else if (isspace(c)) {
    while ((c = Ahdrd_peek(ahdrd, 1)) != EOF && isspace(c))
      Ahdrd_skipRead(ahdrd);
    return Ahdrd_readSpace(ahdrd);
  }
  else if (c == ';') {
    while ((c = Ahdrd_peek(ahdrd, 1)) != EOF && c != '\n')
      Ahdrd_skipRead(ahdrd);
    return Ahdrd_readSpace(ahdrd);
  }
  else if (c == '#' && Ahdrd_peek(ahdrd, 2) == '|') {
    while ((c = Ahdrd_peek(ahdrd, 1)) != EOF
        && (c != '|' || Ahdrd_peek(ahdrd, 2) != '#'))
      Ahdrd_skipRead(ahdrd);
    Ahdrd_skipRead(ahdrd);    // skip '|'
    Ahdrd_skipRead(ahdrd);    // skip '#'
    return Ahdrd_readSpace(ahdrd);
  }
  return ahdrd;
}

char *Ahdrd_readKeyword(Ahdrd *ahdrd) {
  int c;
  Ahdrd_skipRead(ahdrd);    // skip ':'
  while (!isspace((c = Ahdrd_peek(ahdrd, 1))) && c != '(' && c != ')')
    Ahdrd_read(ahdrd);
  return Ahdrd_getToken(ahdrd);
}

static char *Ahdrd_readSurrounded(Ahdrd *ahdrd, char s) {
  int c;
  Ahdrd_skipRead(ahdrd);    // skip surround start
  while ((c = Ahdrd_peek(ahdrd, 1)) != s) {
    if (c == EOF)
      return NULL;
    Ahdrd_read(ahdrd);
    if (c == '\\')
      Ahdrd_read(ahdrd);
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
  int c;
  while (!isspace((c = Ahdrd_peek(ahdrd, 1))) && c != '(' && c != ')')
    Ahdrd_read(ahdrd);
  return Ahdrd_getToken(ahdrd);
}

char *Ahdrd_readNumber(Ahdrd *ahdrd) {
  return Ahdrd_readSymbol(ahdrd);
}

int Ahdrd_isNumber(Ahdrd *ahdrd) {
  int i, c;
  i = 0;
  if ((!isdigit((c = Ahdrd_peek(ahdrd, ++i)))
        && c != '+' && c != '-') || c == '0')
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
