/*
  paren standard library.
*/

extern void *xmalloc(int size);
extern void *xrealloc(void *p, int size);
#define xvstrcat(...) _xvstrcat(__VA_ARGS__, NULL)
extern char *_xvstrcat(char *s1, ...);    // use xvstrcat!
