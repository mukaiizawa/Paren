/*
  paren standard library.
*/

#define MAX_STR_LEN 256

extern void *xmalloc(int size);
char *xmkstr(int size, char *fmt, ...);
