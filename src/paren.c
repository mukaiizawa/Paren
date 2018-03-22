// paren main routine.

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "std.h"
#include "lex.h"

/*
 * paren bnf
 * <s_expr> ::= <list> | <atom>
 * <list> ::= <pure_list> | <dot_list>
 * <pure_list> ::= '(' [<s_expr>] ... ')'
 * <dot_list> ::= '(' <s_expr> ... '.' <s_expr> ')'
 * <atom> ::= <number> | <symbol> | <keyword>
 * <number> ::= [<digit>+ 'x'] [0-9a-z]+ | <digit>+ [ '.' <digit>+ ]
 * <symbol> ::= <identifier>
 * <keyword> ::= ':' <identifier>
 * <identifier> ::= <identifier_first> [<identifier_rest>] ...
 * <identifier_first> ::= [!$%&*+-/\-<=>?a-zA-Z_]
 * <identifier_rest> ::= <identifier_first> | [0-9]
 * <digit> ::= [0-9]
 */

typedef union s_expr *object;

union s_expr {
  int header;
  struct cons {
    int header;
    union s_expr *car, *cdr; 
  } cons;
  struct xint {
    int header;
    int64_t val; 
  } xint;
  struct xfloat {
    int header;
    double val; 
  } xfloat;
  struct symbol {
    int header;
    char *name; 
    union s_expr *val; 
  } symbol;
  struct keyword {
    int header;
    char *name; 
  } keyword;
};

object object_alloc(int size)
{
  return xmalloc(size);
}

int main(int argc, char *argv[])
{
  setbuf(stdout, NULL);
  // object o1, o2;
  // o2 = NULL;
  // o1 = object_alloc(sizeof(struct keyword));
  // o1->header = 3;
  // printf("%d\n", o1->header);
  lex_start(stdin);
  printf("%d\n", lex());
  return 0;
}
