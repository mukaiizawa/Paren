/*
  ring buffer.
*/

#include "std.h"
#include "ringbuf.h"

void ringbuf_reset(struct ringbuf *rb) {
  rb->inpos=0;
  rb->outpos=0;
}

void ringbuf_put(struct ringbuf *rb,int ch) {
  int next;
  next=(rb->inpos+1) % RINGBUF_SIZE;
  if(next==rb->outpos)
    xerror("ringbuf_put: buffer over flow.");
  rb->buf[rb->inpos]=ch;
  rb->inpos=next;
}

int ringbuf_get(struct ringbuf *rb) {
  int ch;
  ch=rb->buf[rb->outpos];
  rb->outpos=(rb->outpos+1) % RINGBUF_SIZE;
  return ch;
}

int ringbuf_empty_p(struct ringbuf *rb) {
  return rb->inpos==rb->outpos;
}
