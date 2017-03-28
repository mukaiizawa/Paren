/*
  ahead reader.
*/

#include <stdio.h>
#include "std.h"
#include "ahdrd.h"

void ahdrd_init(Ahdrd* this, FILE *_fp) {
  this->pos = 0;
  this->fp = _fp;
}

char ahdrd_getch(Ahdrd* this) {
  int i = this->pos;
  if (i == 0)
    return fgetc(this->fp);
  else {
    this->pos = i - 1;
    return this->buf[i];
  }
}

char ahdrd_nextch(Ahdrd* this) {
  char c;
  if (this->pos == AHDRD_BUFSIZE)
    xerror("ahdrd_nextch: Buffer over flow.");
  if (this->buf[this->pos] == EOF)
    xerror("ahdrd_nextch: Reached EOF.");
  c = fgetc(this->fp);
  this->pos = this->pos + 1;
  this->buf[this->pos] = c;
  return c;
}
