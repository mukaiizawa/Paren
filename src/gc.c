// garbage collector

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

#include "std.h"

#include <string.h>

#include "om.h"
#include "ip.h"
#include "gc.h"

/* object construct and regist */

object gc_object_new(object xclass,int ext)
{
  return 0;
}

/* refnew stack */

void gc_refer(object from,object to)
{
}

void gc_regist_refnew(object o)
{
}

/* gc */

int gc_mark(object o)
{
  return 0;
}

void gc(void)
{
}

void gc_full(void)
{
}

void gc_chance(void)
{
}

void gc_init(void)
{
}

object gc_string(char *s)
{
  return NULL;
}
