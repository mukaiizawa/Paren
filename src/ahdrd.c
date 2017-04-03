/*
  ahead reader.
*/

#include <stdio.h>
#include <stdlib.h>

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
  if (this->pos == AHDRD_BUFSIZE) {
    fprintf(stderr, "Queue_dequeue: No such element.");
    exit(1);
  }
  if (this->buf[this->pos] == EOF) {
    fprintf(stderr, "Queue_dequeue: No such element.");
    exit(1);
  }
  c = fgetc(this->fp);
  this->pos = this->pos + 1;
  this->buf[this->pos] = c;
  return c;
}

char ahdrd_peekch(Ahdrd* this, int n) {
  char c;
  if (this->pos == AHDRD_BUFSIZE) {
    fprintf(stderr, "Queue_dequeue: No such element.");
    exit(1);
  }
  if (this->buf[this->pos] == EOF) {
    fprintf(stderr, "Queue_dequeue: No such element.");
    exit(1);
  }
  c = fgetc(this->fp);
  this->pos = this->pos + 1;
  this->buf[this->pos] = c;
  return c;
}
