/*
  lexical analysis.
*/

#ifndef LEX
#define LEX

#include <stdio.h>

#include "prim.h"

extern void Lex_init();
extern S *Lex_parse();
extern S *Lex_parseS();

#endif
