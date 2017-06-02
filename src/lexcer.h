/*
  paren lexcer.
*/

#include "ahdrd.h"

typedef struct {
  FILE *fp;
  Ahdrd ahdrd;
} Lexcer;

extern void Lexcer_init(Lexcer *lex, FILE *fp);
extern S *Lexcer_parseExpr(Lexcer *lex);
