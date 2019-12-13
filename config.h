// paren configure.

// archtecture and compiler

#define OS_LINUX 0
#define OS_ANDROID 1
#define OS_MACOSX 2
#define OS_WINDOWS 10

#ifdef __linux
#ifdef ANDROID
#define OS_CODE OS_ANDROID
#define XCONSOLE_P TRUE
#else
#define OS_CODE OS_LINUX
#endif
#endif

#ifdef __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__
#define OS_CODE OS_MACOSX
#endif

#ifdef _MSC_VER
#define OS_CODE OS_WINDOWS
#define _CRT_SECURE_NO_WARNINGS
#pragma warning(disable:4267)
#endif

#ifdef __MINGW32__
#define OS_CODE OS_WINDOWS
#define __MSVCRT_VERSION__ 0x800
#define WINVER 0x500
#endif

#define UNIX_P (OS_CODE / 10 == 0)
#define WINDOWS_P (OS_CODE == OS_WINDOWS)

#if WINDOWS_P
#ifndef WSTAT64_P
#define WSTAT64_P TRUE
#endif
#endif

#ifdef __GNUC__
#if (__GNUC__==4&&__GNUC_MINOR__>=6)||__GNUC__>=5
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
#endif


// runtime

#define IP_POLLING_INTERVAL 256
#define CALL_STACK_SIZE (1 * 1024)
#define GC_CHANCE_MEMORY (512 * 1024 * 1024)
#define GC_LOG_P FALSE
