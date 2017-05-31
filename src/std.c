/*
  paren standard library.
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>

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

void xstrncat(char *buf, char *s) {
  int n;
  n = MAX_STR_LEN - strlen(buf);
  strncat(buf, s, n);
}

void xsnprintf(char *buf, char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vsnprintf(buf, MAX_STR_LEN, fmt, args);
  va_end(args);
}
