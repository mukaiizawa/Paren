/*
  ring buffner header.
*/

#ifndef RINGBUF
#define RINGBUF

#define RINGBUF_SIZE 128

struct ringbuf {
  unsigned char buf[RINGBUF_SIZE];
  int inpos;
  int outpos;
};

extern void ringbuf_reset(struct ringbuf *rb);
extern void ringbuf_put(struct ringbuf *rb,int ch);
extern int ringbuf_get(struct ringbuf *rb);
extern int ringbuf_empty_p(struct ringbuf *rb);

#endif
