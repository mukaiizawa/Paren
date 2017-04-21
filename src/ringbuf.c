/*
   ring buffer.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ringbuf.h"

Ringbuf *Ringbuf_new() {
  Ringbuf *ringbuf;
  if ((ringbuf = (Ringbuf *)calloc(1, sizeof(Ringbuf))) == NULL) {
    fprintf(stderr, "Ringbuf_new: Cannot allocate memory.");
    exit(1);
  }
  ringbuf->in = 0;
  ringbuf->out = 0;
  return ringbuf;
}

int Ringbuf_isEmpty(Ringbuf *ringbuf) {
  return ringbuf->in == ringbuf->out;
} 

void Ringbuf_put(Ringbuf *ringbuf, int c) {
  int nextIn;
  nextIn = (ringbuf->in + 1) % RINGBUF_BUFSIZ;
  if (nextIn == ringbuf->out) {
    fprintf(stderr, "Ringbuf_put: Buffer over flow.");
    exit(1);
  }
  ringbuf->buf[ringbuf->in] = c;
  ringbuf->in = nextIn;
}

int Ringbuf_get(Ringbuf *ringbuf) {
  int c;
  if (Ringbuf_isEmpty(ringbuf)) {
    fprintf(stderr, "Ringbuf_get: Buffer empty.");
    exit(1);
  }
  c = ringbuf->buf[ringbuf->out];
  ringbuf->out = (ringbuf->out + 1) % RINGBUF_BUFSIZ;
  return c;
}

int Ringbuf_size(Ringbuf *ringbuf) {
  return (ringbuf->out <= ringbuf->in)?
    ringbuf->in - ringbuf->out:
    RINGBUF_BUFSIZ - ringbuf->out - ringbuf->in;
}

void Ringbuf_dump(Ringbuf *ringbuf) {
  int i;
  printf("ringbuf[");
  for (i = ringbuf->out; i != ringbuf->in; i++) {
    printf("%c", ringbuf->buf[i]);
    if (i + 1 != ringbuf->in)
      printf(", ");
  }
  printf("]\n");
}
