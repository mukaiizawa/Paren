/*
  paren standard library.
*/

#define MAX_STR_LEN 256

extern void *xmalloc(int size);
extern void xstrncat(char *buf, char *s);
extern void xsnprintf(char *buf, char *fmt, ...);
