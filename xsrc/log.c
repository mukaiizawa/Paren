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

#include "std.h"
#include "log.h"

static FILE *fp=NULL;

void log_open(char *fn)
{
  if(fn==NULL) fp=stdout;
  else {
    if((fp=fopen(fn,"w"))==NULL) xerror("log_open/open %s failed.",fn);
  }
}

int log_p(void)
{
  return fp!=NULL;
}

void log_c(int ch)
{
  if(log_p()) fputc(ch,fp);
}

void log_ln(void)
{
  log_c('\n');
}

void log_f(char *fmt,...)
{
  va_list va;
  if(log_p()) {
    va_start(va,fmt);
    vfprintf(fp,fmt,va);
    va_end(va);
  }
}

void log_close(void)
{
  if(log_p()) {
    if(fp!=stdout) fclose(fp);
    fp=NULL;
  }
}
