/*
  ring buffer.
*/

#ifndef RINGBUF
#define RINGBUF

#define RINGBUF_BUFSIZ 100

struct Ringbuf {
  int in;
  int out;
  int buf[RINGBUF_BUFSIZ];
};

extern void Ringbuf_init(struct Ringbuf *ringbuf);
extern int Ringbuf_isEmpty(struct Ringbuf *ringbuf);
extern void Ringbuf_put(struct Ringbuf *ringbuf, int c);
extern int Ringbuf_get(struct Ringbuf *ringbuf);
extern int Ringbuf_size(struct Ringbuf *ringbuf);
extern void Ringbuf_dump(struct Ringbuf *ringbuf);

#endif
