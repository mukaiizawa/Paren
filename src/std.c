/*
  paren standard library.
*/

#include <stdio.h>
#include <stdlib.h>
#include "std.h"

void xerror(char *buf) {
  fprintf(stderr,"%s\n", buf);
  exit(1);
}
