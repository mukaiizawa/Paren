/*
  queue.
*/

#include <stdio.h>
#include <stdlib.h>

#include "queue.h"

void Queue_init(Queue* q) {
  q->top = NULL;
  q->tail = NULL;
}

void Queue_enqueue(Queue* q, char* str) {
  struct Queue_node *new_node;
  new_node = malloc(sizeof(*new_node));
  new_node->val = str;
  new_node->next = NULL;
  if (q->top == NULL)
    q->top = q->tail = new_node;
  else 
    q->tail = q->tail->next = new_node;
}

char* Queue_dequeue(Queue* q) {
  if (q->top == NULL) {
    fprintf(stderr, "Queue_dequeue: No such element.");
    exit(1);
  }
  struct Queue_node *temp = q->top;
  q->top = temp->next;
  char* ret_value = temp->val;
  free(temp);
  return ret_value;
}

void Queue_dump(Queue* q) {
  struct Queue_node *node;
  printf("queue: [");
  for (node = q->top; node; node = node->next)
    printf(" %s", node->val);
  puts(" ]");
}
