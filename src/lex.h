// lexical analysis.

#include "xbarray.h"

// token types -- -1..255 as single character
#define LEX_SYMBOL 256
#define LEX_KEYWORD 257
#define LEX_INT 258
#define LEX_FLOAT 259

extern struct xbarray lex_str;
extern int lex_ival;
extern double lex_fval;

extern void lex_start(FILE *fp);
extern void lex_error(char *fmt,...);
extern int lex(void);

extern char *lex_token_name(char *buf,int tk);
extern char *lex_current_string(char *buf,int tk);
