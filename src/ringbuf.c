/*
   ring buffer.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "ringbuf.h"

void Ringbuf_init(Ringbuf *ringbuf) {
  ringbuf->in = 0;
  ringbuf->out = 0;
}

int Ringbuf_isEmpty(Ringbuf *ringbuf) {
  return ringbuf->in == ringbuf->out;
} 

void Ringbuf_put(Ringbuf *ringbuf, int c) {
  int nextIn;
  nextIn = (ringbuf->in + 1) % RINGBUF_BUFSIZ;
  assert(nextIn != ringbuf->out);    // buffer  over flow.
  ringbuf->buf[ringbuf->in] = c;
  ringbuf->in = nextIn;
}

int Ringbuf_get(Ringbuf *ringbuf) {
  int c;
  assert(!Ringbuf_isEmpty(ringbuf));
  c = ringbuf->buf[ringbuf->out];
  ringbuf->out = (ringbuf->out + 1) % RINGBUF_BUFSIZ;
  return c;
}

int Ringbuf_size(Ringbuf *ringbuf) {
  return (ringbuf->out <= ringbuf->in)?
    ringbuf->in - ringbuf->out:
    RINGBUF_BUFSIZ - ringbuf->out - ringbuf->in;
}
