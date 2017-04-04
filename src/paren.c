/*
  paren main routine.
*/

#include <stdio.h>

char prompt[1000];

char *read() {
 return fgets(prompt, sizeof(prompt), stdin);
}

char *eval() {
}

char *print() {
}

int main(int argc, char* argv[]) {
  while (1) {
    fputs(") ", stdout);
    printf("%s", read());
    fflush(stdout);
  }
  return 0;
}
