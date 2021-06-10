// xc std.

#define TRUE 1
#define FALSE 0
#define MAX_STR_LEN 256

#include "config.h"

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <utime.h>

#if UNIX_P
#include <arpa/inet.h>
#include <dlfcn.h>
#include <netinet/in.h>
#include <signal.h>
#include <sys/socket.h>
#endif

#if WINDOWS_P
#include <fcntl.h>
#include <io.h>
#include <mbctype.h>
#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>
#endif

extern void xvsprintf(char *buf, char *fmt, va_list va);
extern void xsprintf(char *buf, char *fmt, ...);
extern void xerror(char *fmt, ...);
extern void *xmalloc(int size);
extern void xfree(void *);
extern void *xrealloc(void *p, int size);
extern char *xstrdup(char *s);
#if WINDOWS_P
extern int xwctomb(LPWSTR lp, char *p);
extern int xmbtowc(char *p, LPWSTR lp);
extern int xmbtombacp(char *p, char *q);
#endif

#ifdef NDEBUG
#define xassert(cond) ;
#else
extern void xassert_failed(char *fn, int line);
#define xassert(cond) if (!(cond)) xassert_failed(__FILE__, __LINE__);
#endif
