#include <assert.h>

#include "ringbuf.h"

int main(void) {
  struct Ringbuf *ringbuf;
  ringbuf = Ringbuf_new();
  assert(Ringbuf_isEmpty(ringbuf));
  assert(Ringbuf_size(ringbuf) == 0);
  Ringbuf_put(ringbuf, '0');    // 0
  assert(!Ringbuf_isEmpty(ringbuf));
  assert(Ringbuf_get(ringbuf) == '0');    //
  assert(Ringbuf_isEmpty(ringbuf));
  Ringbuf_put(ringbuf, '0');    // 0
  assert(Ringbuf_size(ringbuf) == 1);
  Ringbuf_put(ringbuf, '1');    // 0 1
  assert(Ringbuf_size(ringbuf) == 2);
  Ringbuf_put(ringbuf, '2');    // 0 1 2
  assert(Ringbuf_size(ringbuf) == 3);
  assert(Ringbuf_get(ringbuf) == '0');    // 1 2
  assert(Ringbuf_get(ringbuf) == '1');    // 2
  assert(Ringbuf_get(ringbuf) == '2');    //
  assert(Ringbuf_size(ringbuf) == 0);
  return 0;
}
