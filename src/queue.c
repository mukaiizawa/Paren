#include <stdio.h>
#include <stdlib.h>

typedef struct {
  struct Queue_node {
    int value;
    struct Queue_node *next;
  } *top, *tail;
} Queue;

void Queue_put(Queue*, int);
int Queue_get(Queue*);
void Queue_print(Queue*);

void Queue_put(Queue* q, int n) {
  struct Queue_node *new_node = malloc(sizeof(*new_node));
  new_node->value = n;
  new_node->next = NULL;
  if (q->top == NULL)
    q->top = q->tail = new_node;
  else 
    q->tail = q->tail->next = new_node;
}

int Queue_get(Queue* q) {
  if (q->top == NULL) {
    fprintf(stderr, "queque is empty. can't get\n");
    exit(-1);
  }
  struct Queue_node *temp = q->top;
  q->top = temp->next;
  int ret_value = temp->value;
  free(temp);
  return ret_value;
}

void Queue_print(Queue* q) {
  struct Queue_node *node;
  printf("queue: [");
  for(node = q->top; node; node = node->next)
    printf(" %d", node->value);
  puts(" ]");
}

int main(void){
  int i,n;
  Queue q = { NULL, NULL};
  srand(1);
  for (i = 0; i < 3; i++) {
    n = rand() % 100;
    printf("enqueue %d\n", n);
    Queue_put(&q, n);
  }
    Queue_print(&q);
  for(i = 0; i < 3; i++) {
    printf("dequeue %d\n", Queue_get(&q));
    Queue_print(&q);
  }
  return 0;
}
