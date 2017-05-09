/*
  s-expression.
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>

#include "sexpr.h"

S *t;
S *nil;

static char *TYPE_STRING[10] = {
  "Cons",
  "Map",
  "Symbol",
  "Keyword",
  "String",
  "Character",
  "Number",
  "Function",
  "Stream",
  "Error"
};

char *Type_asString(Type type) {
  return TYPE_STRING[type];
};

static S *S_alloc() {
  S *expr;
  if ((expr = (S *)calloc(1, sizeof(S))) == NULL) {
    fprintf(stderr, "S_alloc: Cannot allocate memory.");
    exit(1);
  }
  return expr;
}

S *Cons_new(S *car, S *cdr) {
  S *prev;
  if (ATOMP(cdr) && !NILP(cdr)) {
    fprintf(stderr, "Cons: Do not allow create cons cell without terminated nil.");
    exit(1);
  }
  prev = S_alloc();
  prev->Cons.type = Cons;
  FIRST(prev) = car;
  REST(prev) = cdr;
  return prev;
}

static struct MapNode *MapNode_alloc() {
  struct MapNode *mapNode;
  if ((mapNode = (struct MapNode *)calloc(1, sizeof(struct MapNode))) == NULL) {
    fprintf(stderr, "MapNode_alloc: Cannot allocate memory.");
    exit(1);
  }
  return mapNode;
}

S *Map_new(S *car, S *cdr) {
  S *expr;
  expr = S_alloc();
  expr->Map.type = Map;
  expr->Map.head = MapNode_alloc();
  return expr;
}

S *Symbol_new(char *val) {
  S *expr;
  expr = S_alloc();
  expr->Symbol.type = Symbol;
  expr->Symbol.val = val;
  return expr;
}

S *Keyword_new(char *val) {
  S *expr;
  expr = S_alloc();
  expr->Keyword.type = Keyword;
  expr->Keyword.val = val;
  return expr;
}

S *String_new(char *val) {
  S *expr;
  expr = S_alloc();
  expr->String.type = String;
  expr->String.val = val;
  return expr;
}

S *Character_new(char val) {
  S *expr;
  expr = S_alloc();
  expr->Character.type = Character;
  expr->Character.val = val;
  return expr;
}

S *Number_new(double val) {
  S *expr;
  expr = S_alloc();
  expr->Number.type = Number;
  expr->Number.val = val;
  return expr;
}

S *Function_new(S *f(S *), S *args) {
  S *expr;
  expr = S_alloc();
  expr->Function.type = Function;
  expr->Function.f = f;
  expr->Function.args = args;
  return expr;
}

S *Stream_new(FILE *stream) {
  S *expr;
  expr = S_alloc();
  expr->Stream.type = Stream;
  expr->Stream.stream = stream;
  return expr;
}

S *Error_new(char *str) {
  S *expr;
  expr = S_alloc();
  expr->Error.type = Error;
  expr->Error.val = str;
  return expr;
}

extern S *Lex_parseExpr();

S *S_read() {
  return Lex_parseExpr();
}

S *S_eval(S *expr, S *env) {
  S *root, *cmd, *args;
  if (ATOMP(expr))
    return (expr->Symbol.type == Symbol)?
      // Map_lookup(env, expr->Symbol.val)
      expr:
      expr;
  root = expr;
  while (!NILP(expr)) {
    expr->Cons.car = S_eval(expr->Cons.car, env);
    expr = expr->Cons.cdr;
  }
  cmd = root->Cons.car;
  args = root->Cons.cdr;
  if (cmd->Function.type != Function) {
    return Error_new("eval: undefined function.");
  }
  // invoke primitive function.
  if (cmd->Function.f != NULL) {
    return (cmd->Function.f)(args);
  }
  // invoke user defined function.
  else {
    return nil;    // TODO: apply user defined function.
  }
}

S *S_print(S *expr) {
  int type;
  if (ATOMP(expr)) {
    if (expr->Cons.type == Number) {
      double intptr, fraction;
      fraction = modf(expr->Number.val, &intptr);
      if (fraction == 0)
        fprintf(stdout, "%d", (int)intptr);
      else
        fprintf(stdout, "%f", expr->Number.val);
    }
    else if (expr->Cons.type == Character)
      fprintf(stdout, "%c", expr->Character.val);
    else if (expr->Cons.type == Function)
      fprintf(stdout, "%d", expr->Cons.type);
    else
      fprintf(stdout, "%s", expr->String.val);
  }
  else {
    fprintf(stdout, "(");
    S_print(expr->Cons.car);
    for (expr = expr->Cons.cdr; !NILP(expr); expr = expr->Cons.cdr) {
      fprintf(stdout, ATOMP(expr->Cons.car)? " ": "");
      S_print(expr->Cons.car);
    }
    fprintf(stdout, ")");
  }
  fflush(stdout);
  return expr;
}

int S_length(S *expr) {
  int count;
  count = 1;
  if (expr->Cons.type != Cons)
    return count;
  else {
    while (!NILP((expr = REST(expr))))    // safe?
      count++;
    return count;
  }
}

S *S_reverse(S *expr) {
  S *root;
  if (NILP(expr))
    return nil;
  assert(expr->Cons.type == Cons);
  root = nil;
  while (!NILP(expr)) {
    root = Cons_new(FIRST(expr), root);
    expr = REST(expr);
  }
  return root;
}
