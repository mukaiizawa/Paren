#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "ahdrd.h"

// assume input from stdin with '012345'
int main(void) {
  Ahdrd *ahdrd;
  ahdrd = Ahdrd_new(stdin);
  assert(Ahdrd_read(ahdrd) == '0');
  assert(Ahdrd_peek(ahdrd, 1) == '1');
  assert(Ahdrd_peek(ahdrd, 2) == '2');
  assert(strcmp(Ahdrd_getToken(ahdrd), "0") == 0);
  assert(Ahdrd_read(ahdrd) == '1');
  assert(Ahdrd_read(ahdrd) == '2');
  assert(Ahdrd_read(ahdrd) == '3');
  assert(strcmp(Ahdrd_getToken(ahdrd), "123") == 0);
  return 0;
}
