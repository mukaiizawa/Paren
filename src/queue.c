#include <stdio.h>
#include <stdlib.h>

typedef struct {
    struct node {
        int value;
        struct node *next;
    } *top, *tail;
} Queue;

void put(Queue*, int);
int get(Queue*);
void printQueue(Queue*);

void put(Queue* q, int n){
  struct node *new_node = malloc(sizeof(*new_node));
  new_node->value = n;
  new_node->next = NULL;
  if(q->top == NULL)
    q->top = q->tail = new_node;
  else 
    q->tail = q->tail->next = new_node;
}

int get(Queue* q){
    if(q->top == NULL){
        fprintf(stderr, "queque is empty. can't get\n");
        exit(-1);
    }
    struct node *temp = q->top;
    q->top = temp->next;
    int ret_value = temp->value;
    free(temp);
    return ret_value;
}

void printQueue(Queue* q){
    struct node *temp = q->top;
    printf("queue:");
    while(temp){
        printf(" %d", temp->value);
        temp = temp->next;
    }
    puts("");
}

int main(void){
    int i,n;
    Queue q = { NULL, NULL};
    srand(1);
    for (i = 0; i < 3; i++) {
        n = rand() % 100;
        printf("enqueue %d\n", n);
        put(&q, n);
        printQueue(&q);
    }
    for(i = 0; i < 3; i++) {
        printf("dequeue %d\n", get(&q));
        printQueue(&q);
    }
    return 0;
}

