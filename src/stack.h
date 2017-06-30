/*
  stack.
*/

typedef struct {
  struct StackNode {
    struct StackNode *next;
    void *val;
  } *head, *pool;
} Stack;

extern void Stack_init(Stack *stack);
extern void Stack_push(Stack *stack, void *val);
extern void *Stack_pop(Stack *stack);
extern int Stack_isEmpty(Stack *stack);
extern void Stack_free(Stack *stack);
