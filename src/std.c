/*
  paren standard library.
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>

#include "std.h"

#define xerror(str) { \
  fprintf(stderr, str); \
  exit(1); \
}

void *xmalloc(int size) {
  void *p;
  assert(size > 0);
  if ((p = malloc(size)) == NULL) xerror("xmalloc: Cannot allocate memory.");
  return p;
}

void *xrealloc(void *p, int size) {
  if (p == NULL) return xmalloc(size);
  else if ((p = realloc(p, size)) == NULL)
    xerror("xmalloc: Cannot allocate memory.");
  return p;
}

char *_xvstrcat(char *s1, ...) {
  int len;
  char *s, *buf;
  va_list args;
  va_start(args, s1);
  buf = xmalloc(len = sizeof(char));
  buf[0] = '\0';
  for (s = s1; s != NULL; s = va_arg(args, char *)) {
    xrealloc(buf, len += (strlen(s) + 1));
    buf = strcat(buf, s);
  }
  va_end(args);
  return buf;
}
