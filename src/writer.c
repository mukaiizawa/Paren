/*
  paren writer.
*/

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "splay.h"
#include "env.h"
#include "paren.h"
#include "writer.h"

void Writer_init(Writer *wr, FILE *fp) {
  wr->fp = fp;
}

void Writer_write(Writer *wr, S *expr) {
  if (TYPEP(expr, Symbol)) fprintf(wr->fp, "%s", expr->Symbol.name);
  else if (TYPEP(expr, Keyword)) fprintf(wr->fp, ":%s", expr->Keyword.val);
  else if (TYPEP(expr, String)) fprintf(wr->fp, "%s", expr->String.val);
  else if (TYPEP(expr, Char)) fprintf(wr->fp, "%c", expr->Char.val);
  else if (TYPEP(expr, Number)) {
    double intptr, fraction;
    fraction = modf(expr->Number.val, &intptr);
    if (fraction == 0) fprintf(wr->fp, "%d", (int)intptr); 
    else fprintf(wr->fp, "%f", expr->Number.val);
  }
  else if (TYPEP(expr, Function)) fprintf(wr->fp, "%c", expr->Char.val);
  else if (TYPEP(expr, Error)) fprintf(wr->fp, "%s", expr->Error.val);
  else if (TYPEP(expr, Cons)) {
    while (!NILP(expr)) {
      Writer_write(wr, FIRST(expr));
      if (!NILP(expr = REST(expr))) fprintf(wr->fp, " ");
    }
  }
  else fprintf(wr->fp, "<%s: %p>", expr->Type.type->Keyword.val, expr);
}

FILE *Writer_getFp(Writer *wr) {
  return wr->fp;
}

void Writer_setFp(Writer *wr, FILE *fp) {
  wr->fp = fp;
}
