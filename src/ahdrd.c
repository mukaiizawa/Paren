/*
  ahead reader.
*/

#include <stdio.h>
#include "std.h"

void ahdrd_init(AheadReader* this, FILE *_fp) {
  this->pos = 0;
  this->fp = _fp;
  this->buf = 
}

char ahdrd_getch(AheadReader* this) {
  int i = this->pos;
  if (i == 0)
    return fgetc(this->fp);
  else {
    this->pos = i - 1;
    return this->buf[i];
  }
}

char ahdrd_nextch(AheadReader* this) {
  char c;
  if (this->pos == AHDRD_BUFSIZE)
    xerror("ahdrd_nextch: Buffer over flow.");
  c = fgetc(this->fp);
  this->pos = this->pos + 1;
  this->buf[this->pos] = c;
  return c;
}
