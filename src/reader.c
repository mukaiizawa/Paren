/*
  paren reader.
*/

#include <stdio.h>

static int BUF_SIZE = 2048;
static char prompt[BUF_SIZE];

char* read_paren() {
  char* s = fgets(prompt, BUF_SIZE, stdin);
  return fgets(prompt, BUF_SIZE, stdin);
}

char* read() {
  char* s = fgets(prompt, BUF_SIZE, stdin);
  return fgets(prompt, BUF_SIZE, stdin);
}

int main(int argc, char* argv[]) {
  while (1) {
    fputs(") ", stdout);
    printf("%s", read());
  }
  return 0;
}

