#include<stdio.h>
#include<stdlib.h>

enum type {
  Symbol,
  Keyword,
  String,
  Character,
  Number,
  Function,
  Cons
};

struct S {
  enum type type;
  union {
    struct {
      struct S *prev, *car, *cdr;
    } Cell;
    int number;
    char *string;
    char character;
  } object;
}; 

#define defalloc(type) \
  struct type *type ## _new() { \
    struct type *obj; \
    if ((obj = (struct type *)calloc(1, sizeof(struct type))) == NULL) { \
      fprintf(stderr, "%s_new: Cannot allocate memory.", #type); \
      exit(1); \
    } \
    return obj; \
  }

defalloc(S);

int main(void) {
  struct S *o;
  o = S_new();
  o->type = Character;
  printf("%d", o->type);
  return 0;
}
