/*
  ring buffner header.
*/

#ifndef RINGBUF
#define RINGBUF

#define RINGBUF_MAX 128

struct ringbuf {
  unsigned char buf[RINGBUF_MAX];
  int inpos;
  int outpos;
};

extern void ringbuf_reset(struct ringbuf *rb);
extern void ringbuf_put(struct ringbuf *rb, int ch);
extern int ringbuf_getc(struct ringbuf *rb);
extern char* ringbuf_getall(struct ringbuf *rb, char* buf);
extern int ringbuf_size(struct ringbuf *rb);
extern int ringbuf_empty_p(struct ringbuf *rb);

#endif
