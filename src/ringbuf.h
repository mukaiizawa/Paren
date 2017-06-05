/*
  ring buffer.
*/

#define RINGBUF_BUFSIZ 100

typedef struct {
  int in;
  int out;
  int buf[RINGBUF_BUFSIZ];
} Ringbuf;

extern void Ringbuf_init(Ringbuf *ringbuf);
extern int Ringbuf_isEmpty(Ringbuf *ringbuf);
extern void Ringbuf_put(Ringbuf *ringbuf, int c);
extern int Ringbuf_get(Ringbuf *ringbuf);
extern int Ringbuf_size(Ringbuf *ringbuf);
