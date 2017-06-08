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
  FILE *fp;
  fp = wr->fp;
  if (TYPEP(expr, Symbol)) fprintf(fp, "%s", expr->Symbol.name);
  else if (TYPEP(expr, Keyword)) fprintf(fp, ":%s", expr->Keyword.val);
  else if (TYPEP(expr, String)) fprintf(fp, "%s", expr->String.val);
  else if (TYPEP(expr, Char)) fprintf(fp, "%c", expr->Char.val);
  else if (TYPEP(expr, Number)) {
    double intptr, fraction;
    fraction = modf(expr->Number.val, &intptr);
    if (fraction == 0) fprintf(fp, "%d", (int)intptr); 
    else fprintf(fp, "%f", expr->Number.val);
  }
  // TODO: writer for function.
  // else if (TYPEP(expr, Function)) fprintf(fp, "%p", expr);
  else if (TYPEP(expr, Error)) fprintf(fp, "%s", expr->Error.val);
  else if (TYPEP(expr, Cons)) {
    fprintf(fp, "(");
    while (!NILP(expr)) {
      Writer_write(wr, FIRST(expr));
      if (!NILP(expr = REST(expr))) fprintf(fp, " ");
    }
    fprintf(fp, ")");
  }
  else fprintf(fp, "<%s: %p>", expr->Object.type->Keyword.val, expr);
}

FILE *Writer_getFp(Writer *wr) {
  return wr->fp;
}

void Writer_setFp(Writer *wr, FILE *fp) {
  wr->fp = fp;
}
