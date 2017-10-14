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

/* archtecture and compiler */

#define OS_LINUX 0
#define OS_CYGWIN 1
#define OS_ANDROID 2
#define OS_MACOSX 3
#define OS_MINIX 4
#define OS_FREEBSD 5

#define OS_WINDOWS 10
#define OS_DOS 11 /*DJGPP*/

#ifdef __linux
#ifdef ANDROID
#define OS_CODE OS_ANDROID
#define XCONSOLE_P TRUE
#else
#define OS_CODE OS_LINUX
#endif
#endif

#ifdef __CYGWIN__
#define OS_CODE OS_CYGWIN
#endif

#ifdef __MINGW32__
#define OS_CODE OS_WINDOWS
#define __MSVCRT_VERSION__ 0x800
#define WINVER 0x500
#endif

#ifdef _MSC_VER
#define OS_CODE OS_WINDOWS
#define _CRT_SECURE_NO_WARNINGS
#pragma warning(disable:4267)
#endif

#ifdef __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__
#define OS_CODE OS_MACOSX
#define NAMED_SEMAPHORE_P TRUE
#endif

#ifdef __minix
#define OS_CODE OS_MINIX
#endif

#ifdef __FreeBSD__
#define OS_CODE OS_FREEBSD
#endif

#ifdef __DJGPP__
#define OS_CODE OS_DOS
#endif

#ifdef __BORLANDC__
#define OS_CODE OS_WINDOWS
#pragma warn -8004
#pragma warn -8008
#pragma warn -8057
#pragma warn -8064
#define WSTAT64_P FALSE
#endif

#define UNIX_P (OS_CODE/10==0)
#define MINIX_P (OS_CODE==OS_MINIX)
#define CYGWIN_P (OS_CODE==OS_CYGWIN)

#define WINDOWS_P (OS_CODE==OS_WINDOWS)
#define DOS_P (OS_CODE==OS_DOS)

#ifndef XCONSOLE_P
#define XCONSOLE_P FALSE
#endif

#ifndef NAMED_SEMAPHORE_P
#define NAMED_SEMAPHORE_P FALSE
#endif

#if WINDOWS_P
#ifndef WSTAT64_P
#define WSTAT64_P TRUE /* use _wstat64 */
#endif
#endif

#ifdef __GNUC__
#if (__GNUC__==4&&__GNUC_MINOR__>=6)||__GNUC__>=5
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
#endif

/* options for paren */

#define K 1024
#define DEFAULT_FRAME_STACK_SIZE 1 /* * K CELL */
#define DEFAULT_CONTEXT_STACK_SIZE 1 /* * K CELL */

#define IP_POLLING_INTERVAL 256 /* for gc/interrut check */

#define GC_OLD_AMOUNT 256
#define GC_NEW_AMOUNT 256

#ifdef NDEBUG
#define TUNE_P TRUE
#else
#define TUNE_P FALSE
#endif

#define MACRO_OM_P_P TUNE_P
#define MACRO_SINT_P_P TUNE_P
#define MACRO_SINT_VAL_P TUNE_P
#define MACRO_SINT_P TUNE_P
#define MACRO_OM_SIZE_P TUNE_P

#define OM_MEMORY_CHUNK_P TUNE_P

#define IP_METHOD_CACHE_P TUNE_P

/* #define GC_LOG */

#if DOS_P
#define DOSTERM_BIOSKBD_P TRUE
#endif

/** interrupt system */

#define INTR_SIGNAL 0
#define INTR_SIGACTION 1
#define INTR_WINCONSOLE 2
#define INTR_DOSPOLLING 3

#if UNIX_P
#define INTR_CODE INTR_SIGACTION
#endif

#if WINDOWS_P
#define INTR_CODE INTR_WINCONSOLE
#endif

#if DOS_P
#define INTR_CODE INTR_DOSPOLLING
#endif

#define INTR_SIGNAL_P (INTR_CODE==INTR_SIGNAL)
#define INTR_SIGACTION_P (INTR_CODE==INTR_SIGACTION)
#define INTR_WINCONSOLE_P (INTR_CODE==INTR_WINCONSOLE)
#define INTR_DOSPOLLING_P (INTR_CODE==INTR_DOSPOLLING)
