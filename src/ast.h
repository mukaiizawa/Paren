/*
  abstruct syntax tree.
*/

#ifndef AST
#define AST

#define AST_LEAF 0
#define AST_CONS 1
#define AST_NIL 2

#define FIRST(x) x->car
#define REST(x) x->cdr
#define SECOND(x) x->cdr->car
#define THIRD(x) x->cdr->cdr->car

struct Ast {
  int type;
  char *val;
  struct Ast *prev, *car, *cdr;
};

extern int Ast_isAtom(struct Ast *node);
extern int Ast_isNil(struct Ast *node);
extern struct Ast *Ast_alloc();
extern struct Ast *Ast_cons(struct Ast *car, struct Ast *cdr);
extern struct Ast *Ast_consNil(struct Ast *car);
extern void Ast_dump(struct Ast *node);

#endif
