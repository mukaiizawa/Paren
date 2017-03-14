/*
  paren main routine.
*/

#include <stdio.h>

static char prompt[2048];

char* read() {
  return fgets(prompt, 2048, stdin);
}

int main(int argc, char* argv[]) {
  while (1) {
    fputs(") ", stdout);
    printf("%s", read());
  }
  return 0;
}
