// xc std

#define TRUE 1
#define FALSE 0

#include "config.h"

#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STR_LEN 256

extern void xvsprintf(char *buf, char *fmt, va_list va);
extern void xsprintf(char *buf, char *fmt, ...);

extern void xerror(char *fmt, ...);
extern void *xmalloc(int size);
extern void xfree(void *);
extern void *xrealloc(void *p, int size);
extern char *xstrdup(char *s);

#ifdef NDEBUG
#define xassert(cond) ;
#else
extern void xassert_failed(char *fn, int line);
#define xassert(cond) if(!(cond)) xassert_failed(__FILE__, __LINE__);
#endif
