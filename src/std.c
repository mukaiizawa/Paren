/*
  paren standard library.
*/

#include <assert.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  if ((p = realloc(p, size)) == NULL)
    xerror("xrealloc: Cannot allocate memory.");
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
    buf = xrealloc(buf, sizeof(char) * (len += strlen(s)));
    strcat(buf, s);
  }
  va_end(args);
  return buf;
}

char *xitoa(int n) {
  int len;
  char *s;
  assert(n >= 0);    // support only positive number.
  s = xmalloc(sizeof(char) * (len = (int)ceil(log10(n + 1)) + 1));
  if (sprintf(s, "%d", n) + 1 != len) xerror("xiota: Buffer over flow.");
  return s;
}

int xstreq(char *s1, char *s2) {
  return strcmp(s1, s2) == 0;
}

int xnstreq(char *s1, char *s2, int n) {
  return strncmp(s1, s2, n) == 0;
}
