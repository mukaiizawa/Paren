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

#include "xctype.h"

int sjis_trail_size(int ch)
{
  if((0x81<=ch&&ch<=0x9f)||(0xe0<=ch&&ch<=0xfc)) return 1;
  else return 0;
}

int sjis_mblead_p(int ch)
{
  return sjis_trail_size(ch)!=0;
}

int sjis_mbtrail_p(int ch)
{
  return (0x40<=ch&&ch<=0x7e)||(0x80<=ch&&ch<=0xfc);
}

int utf8_trail_size(int ch)
{
  if((ch&0xe0)==0xc0) return 1;
  else if((ch&0xf0)==0xe0) return 2;
  else if((ch&0xf8)==0xf0) return 3;
  else if((ch&0xfc)==0xf8) return 4;
  else if((ch&0xfe)==0xfc) return 5;
  else return 0;
}

int utf8_mblead_p(int ch)
{
  return utf8_trail_size(ch)!=0;
}

int utf8_mbtrail_p(int ch)
{
  return 0x80<=ch&&ch<=0xbf;
}
