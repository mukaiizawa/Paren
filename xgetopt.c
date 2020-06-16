// xc getopt

/* lisence {{{
Mulk system.
Copyright (C) 2009-2020 Ken'ichi Tokuoka. All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
}}} */

#include "std.h"
#include "xgetopt.h"

int xoptind = 0;
char *xoptarg;
static char *cur = NULL;

int xgetopt(int argc, char **argv, char *opt)
{
  char *p;
  if (cur == NULL || *cur == '\0') {
    xoptind++;
    if (xoptind == argc) return EOF;
    cur = argv[xoptind];
    if (*cur != '-') return EOF;
    cur++;
    if (*cur == '\0') return EOF;
  }
  if (*cur == '-') {
    xoptind++;
    return EOF;
  }
  for (p = opt; *p != *cur; p++) {
    if (*p == '\0') {
      fprintf(stderr, "illegal option -- %c.\n", *cur);
      return '?';
    }
  }
  cur++;
  if (*(p + 1) != ':');
  else if (*cur == '\0') {
    xoptind++;
    if (xoptind == argc) {
      fprintf(stderr, "option requires an argument -- %c.\n", *p);
      return '?';
    }
    xoptarg = argv[xoptind];
  } else {
    xoptarg = cur;
    cur = NULL;
  }
  return *p;
}
