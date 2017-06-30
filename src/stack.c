/*
  stack.
*/

#include <assert.h>
#include <stdlib.h>

#include "std.h"
#include "stack.h"

static struct StackNode *StackNode_alloc() {
  return xmalloc(sizeof(struct StackNode));
}

void Stack_init(Stack *stack) {
  stack->head = stack->pool = NULL;
}

void Stack_push(Stack *stack, void *val) {
  struct StackNode *node;
  if (stack->pool == NULL) node = StackNode_alloc();
  else {
    node = stack->pool;
    stack->pool = node->next;
  }
  node->val = val;
  node->next = stack->head;
  stack->head = node;
}

void *Stack_pop(Stack *stack) {
  struct StackNode *node;
  assert(!Stack_isEmpty(stack));
  node = stack->head;
  stack->head = node->next;
  node->next = stack->pool;
  stack->pool = node;
  return stack->pool->val;
}

int Stack_isEmpty(Stack *stack) {
  return stack->head == NULL;
}

void Stack_free(Stack *stack) {
  struct StackNode *s;
  while (stack->pool != NULL) {
    s = stack->pool;
    stack->pool = s->next;
    free(s);
  }
  while (stack->head != NULL) {
    s = stack->head;
    stack->head = s->next;
    free(s);
  }
}
