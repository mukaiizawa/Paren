#include <string.h>
#include <assert.h>
#include "queue.h"

int main(void) {
  Queue q;
  Queue_init(&q);
  Queue_enqueue(&q, "first");
  Queue_enqueue(&q, "second");
  Queue_enqueue(&q, "third");
  assert(strcmp(Queue_dequeue(&q), "first") == 0);
  assert(strcmp(Queue_dequeue(&q), "second") == 0);
  assert(strcmp(Queue_dequeue(&q), "third") == 0);
  return 0;
}
