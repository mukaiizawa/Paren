/*
 *                                 Mulk system.
 *        Copyright (C) 2009-2017 Ken'ichi Tokuoka. All rights reserved.
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

#include "xbarray.h"

#define PF_ERROR 0
#define PF_NONE 1
#define PF_FILE 2
#define PF_DIR 4
#define PF_OTHER 8
#define PF_READABLE 16
#define PF_WRITABLE 32

struct pf_stat {
  int64_t mtime;
  int64_t size;
};

extern int pf_stat(char *fn,struct pf_stat *statbuf);
extern char *pf_exepath(char *argv0,char *path);
extern int pf_utime(char *fn,int64_t mtime);
extern char *pf_getcwd(char *buf);
extern int pf_readdir(char *path,struct xbarray *xba);
extern int pf_mkdir(char *pn);
extern int pf_remove(char *fn);
extern int pf_chdir(char *pn);
