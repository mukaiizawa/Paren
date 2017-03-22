/*
  ring buffer.
*/

#include "std.h"
#include "ringbuf.h"

void ringbuf_reset(struct ringbuf *rb) {
  rb->inpos = 0;
  rb->outpos = 0;
}

void ringbuf_put(struct ringbuf *rb, int c) {
  int next;
  next = (rb->inpos + 1) % RINGBUF_MAX;
  if(next == rb->outpos)
    xerror("ringbuf_put: buffer over flow.");
  rb->buf[rb->inpos] = c;
  rb->inpos = next;
}

int ringbuf_getc(struct ringbuf *rb) {
  int c;
  c = rb->buf[rb->outpos];
  rb->outpos = (rb->outpos + 1) % RINGBUF_MAX;
  return c;
}

char* ringbuf_getall(struct ringbuf *rb, char* buf) {
  int i;
  for (i = 0; !ringbuf_empty_p(rb); i++)
    buf[i] = ringbuf_getc(rb);
  buf[++i] = '\0';
  return buf;
}

int ringbuf_size(struct ringbuf *rb) {
  return (rb->inpos > rb->outpos)?
    rb->inpos - rb->outpos:
    (RINGBUF_MAX - rb->outpos) + rb->inpos;
}

int ringbuf_empty_p(struct ringbuf *rb) {
  return rb->inpos == rb->outpos;
}
