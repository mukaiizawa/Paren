#include "queue.h"

int main(void) {
  int i,n;
  Queue q;
  Queue_init(&q);
  Queue_enqueue(&q, "1");
  Queue_enqueue(&q, "2");
  Queue_enqueue(&q, "3");
  Queue_dump(&q);
  // get
  Queue_dequeue(&q);
  Queue_dump(&q);
  Queue_dequeue(&q);
  Queue_dump(&q);
  Queue_dequeue(&q);
  Queue_dump(&q);
  return 0;
}
