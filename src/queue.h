/*
  lexical analysis.
*/

#ifndef QUEUE
#define QUEUE

typedef struct {
  struct Queue_node {
    char* val;
    struct Queue_node *next;
  } *top, *tail;
} Queue;

void Queue_init(Queue* q);
void Queue_enqueue(Queue* q, char* str);
char* Queue_dequeue(Queue* q);
void Queue_dump(Queue* q);
 
#endif
