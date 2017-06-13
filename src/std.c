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

char *xmkstr(int size, char *fmt, ...) {
  int len;
  char *buf;
  va_list va;
  buf = xmalloc(size);
  va_start(va,fmt);
  if ((len = vsprintf(buf, fmt, va)) < 0 || len >= size) {
    fprintf(stderr, "xmkstr: Buffer over flow.");
    exit(1);
  }
  va_end(va);
  return buf;
}
