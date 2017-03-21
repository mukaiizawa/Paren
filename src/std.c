/*
  paren standard library.
*/

#include <stdlib.h>
#include "std.h"

void xerror(char *buf) {
  fprintf(stderr,"%s\n", buf);
  exit(1);
}
