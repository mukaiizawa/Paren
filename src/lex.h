/*
  lexical analysis.
*/

#ifndef LEX
#define LEX

#include <stdio.h>

#define PAREN_IDENTIFIER 0
#define PAREN_INTEGER 1
#define PAREN_KEYWORD 2
#define PAREN_OPERATOR 3

extern char *lex_parse(FILE *fp);
 
#endif
