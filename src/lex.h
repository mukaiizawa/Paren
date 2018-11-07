// lexical analysis.

#include "xbarray.h"

// token types -- -1..255 as single character
#define LEX_SYMBOL 256
#define LEX_INT 257
#define LEX_FLOAT 258

extern struct xbarray lex_str;
extern int lex_ival;
extern double lex_fval;

extern void lex_start(FILE *fp);
extern void lex_error(char *fmt,...);
extern char *lex_token_name(char *buf,int tk);
extern int lex(void);
