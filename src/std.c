/*
  paren standard library.
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "std.h"

void *xmalloc(int size) {
  void *p;
  assert(size > 0);
  if((p = malloc(size)) == NULL) {
    fprintf(stderr, "xmalloc: Cannot allocate memory.");
    exit(1);
  }
  return p;
}

// void *xstrcat(char *buf, char *s) {
//   int n;
//   n = MAX_STR_LEN - strlen(buf);
//   strncat(buf, s, n);
// }
