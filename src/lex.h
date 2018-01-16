// lexcer.

#include "xbarray.h"

#define LEX_STRING 256
#define LEX_CHARACTER 257
#define LEX_INTEGER 258
#define LEX_IDENTIFIER 259
#define LEX_BINARY_SELECTOR 260
#define LEX_SYMBOL 261
#define LEX_KEYWORD_SELECTOR 262
#define LEX_SPECIAL 263
#define LEX_FLOAT 264

extern struct xbarray lex_str;
extern int lex_ival;
extern double lex_fval;

extern void lex_start(FILE *fp);
extern void lex_error(char *fmt,...);
extern int lex(void);

extern char *lex_token_name(char *buf,int tk);
extern char *lex_current_string(char *buf,int tk);
