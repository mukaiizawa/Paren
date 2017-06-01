/*
  paren writer.
*/

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "splay.h"
#include "paren.h"
#include "writer.h"

void Writer_init(Writer *writer, FILE *fp) {
  writer->fp = fp;
}

void Writer_write(Writer *writer, S *expr) {
  FILE *fp = writer->fp;
  if (TYPEP(expr, Symbol)) fprintf(fp, "%s", expr->Symbol.name);
  if (TYPEP(expr, Keyword)) fprintf(fp, ":%s", expr->Keyword.val);
  if (TYPEP(expr, String)) fprintf(fp, "%s", expr->String.val);
  if (TYPEP(expr, Char)) fprintf(fp, "%c", expr->Char.val);
  if (TYPEP(expr, Number)) {
    double intptr, fraction;
    fraction = modf(expr->Number.val, &intptr);
    if (fraction == 0) fprintf(fp, "%d", (int)intptr); 
    else fprintf(fp, "%f", expr->Number.val);
  }
  if (TYPEP(expr, Function)) fprintf(fp, "%c", expr->Char.val);
  if (TYPEP(expr, Error)) fprintf(fp, "%s", expr->Error.val);
  if (TYPEP(expr, Cons)) {
    while (!NILP(expr)) {
      Writer_write(writer, FIRST(expr));
      if (!NILP(expr = REST(expr))) fprintf(fp, " ");
    }
  }
  else fprintf(fp, "<%s: %p>", expr->Type.type->Keyword.val, expr);
}
