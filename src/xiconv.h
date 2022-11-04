// xiconv.

#if UNIX_P
#define XICONV_UTF8 1
#define XICONV_SJIS 2
#endif

#if WINDOWS_P
#define XICONV_ANSI CP_ACP
#define XICONV_UTF8 CP_UTF8
#define XICONV_SJIS 932
#endif

extern void xiconv_init(void);
extern int xiconv(int from, int to, char *src, char **result);

#if WINDOWS_P
extern int xiconv_wc2mb(int enc, LPWSTR src, char **result);
extern int xiconv_mb2wc(int enc, char *src, LPWSTR *result);
#endif
