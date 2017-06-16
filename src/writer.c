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

static void Writer_writeGeneric(Writer *wr, struct Generic *g) {
  FILE *fp;
  fp = wr->fp;
  fprintf(fp, "\n\t<Generic.%s: ", g->type->Keyword.val);
  fprintf(fp, "(fn ");
  if (g->prim != NULL)
    fprintf(fp, "<BuiltinFunction: 0x%p>)", g->prim);
  else {
    Writer_write(wr, g->params);
    fprintf(fp, " ");
    Writer_write(wr, g->body);
    fprintf(fp, ")");
  }
  fprintf(fp, ">");
}

static void Writer_fmtWrite(Writer *wr, S *args) {
  Writer_write(wr, args);
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
  else if (TYPEP(expr, Function)) {
    struct Generic *g;
    fprintf(fp, "<Function: 0x%p", expr);
    if (expr->Function.gDefault != NULL)
      Writer_writeGeneric(wr, expr->Function.gDefault);
    for (g = expr->Function.generics; g != NULL; g = g->next)
      Writer_writeGeneric(wr, g);
    fprintf(fp, ">");
  }
  else if (TYPEP(expr, Error))
    Writer_fmtWrite(wr, expr->Error.args);
  else if (TYPEP(expr, Cons)) {
    fprintf(fp, "(");
    while (!NILP(expr)) {
      Writer_write(wr, FIRST(expr));
      if (!NILP(expr = REST(expr))) fprintf(fp, " ");
    }
    fprintf(fp, ")");
  }
  else fprintf(fp, "<%s: 0x%p>", expr->Object.type->Keyword.val, expr);
}
