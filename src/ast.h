/*
  abstruct syntax tree.
*/

#ifndef AST
#define AST

#define CONS 0
#define ATOM 1
#define   SYMBOL 2
#define   KEYWORD 3
#define   NUMBER 4
#define     INT 5
#define     DOUBLE 6
#define   CHAR 7
#define   STRING 8

#define FIRST(x) x->car
#define REST(x) x->cdr
#define SECOND(x) x->cdr->car
#define THIRD(x) x->cdr->cdr->car

struct S {
  int type;
  char *val;
  struct S *prev, *car, *cdr;
};

struct S *NIL = NULL;

extern int S_isAtom(struct S *node);
extern struct S *S_cons(struct S *car, struct S *cdr);
extern void S_dump(struct S *node);

#endif
