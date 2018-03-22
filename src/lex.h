/*
	lexical analysis.
	$Id: lex.h 813 2011-07-19 12:30:37Z kt $
*/

#include "xbarray.h"

// token types -- -1..255 as single character
#define tSYMBOL 256
#define tKEYWORD 257
#define tINTEGER 258
#define tFLOAT 259

extern struct xbarray lex_str;
extern int lex_ival;
extern double lex_fval;

extern void lex_start(FILE *fp);
extern void lex_error(char *fmt,...);
extern int lex(void);

extern char *lex_token_name(char *buf,int tk);
extern char *lex_current_string(char *buf,int tk);
