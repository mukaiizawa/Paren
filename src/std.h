// extended standard

/*
 * Mulk system.
 * Copyright (C) 2009-2017 Ken'ichi Tokuoka. All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in 
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#define TRUE 1
#define FALSE 0

#include "config.h"

#include <stdio.h>
#include <stdarg.h>

#ifdef __BORLANDC__
#define INT64(x) (x ## i64)
typedef int intptr_t;
typedef unsigned int uintptr_t;
typedef __int64 int64_t;
#define INT32_MAX 2147483647
#define INT32_MIN (-INT32_MAX-1)
#define INTPTR_MAX INT32_MAX
#define INTPTR_MIN INT32_MIN
#define INT64_MAX INT64(9223372036854775807)
#define INT64_MIN (-INT64_MAX-1)
#else
#define INT64(x) (x ## ll)
#include <stdint.h>
#endif

#define MAX_STR_LEN 256

extern void xvsprintf(char *buf,char *fmt,va_list va);
extern void xsprintf(char *buf,char *fmt,...);

extern void xerror(char *fmt,...);
extern void *xmalloc(int size);
extern void xfree(void *);
extern void *xrealloc(void *p,int size);
extern char *xstrdup(char *s);

/* use xassert to catch assert failure. */

#ifdef NDEBUG
#define xassert(cond) ;
#else
extern void xassert_failed(char *fn,int line);
#define xassert(cond) if(!(cond)) xassert_failed(__FILE__,__LINE__);
#endif
