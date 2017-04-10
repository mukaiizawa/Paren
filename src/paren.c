/*
  paren main routine.
*/

#include <stdio.h>

#include "lex.h"
#include "ast.h"

char prompt[1000];

char *read() {
  Ast_dump(Lex_parse());
  return "read";
}

char *eval() {
  return "eval";
}

char *print() {
  return "print";
}

int main(int argc, char* argv[]) {
  Lex_init();
  while (1) {
    fputs(") ", stdout);
    printf("%s", read());
    fflush(stdout);
  }
  return 0;
}
