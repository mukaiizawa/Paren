/*
  paren main routine.
*/

#include <stdio.h>
#include "lex.h"

char* read() {
 return lex_start(stdin);
}

char* eval() {
}

char* print() {
}

int main(int argc, char* argv[]) {
  while (1) {
    fputs(") ", stdout);
    printf("%s", read());
    fflush(stdout);
  }
  return 0;
}
