/*
  paren primitive.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "std.h"
#include "splay.h"
#include "env.h"
#include "prim.h"

S *t;
S *nil;

/*
 * 型はすべて環境に登録されていて、
 * 型の比較はアドレス比較で済むようにする。
 */
S *Cons;
S *Symbol;
S *Keyword;
S *String;
S *Char;
S *Number;
S *Function;
S *Special;
S *Stream;
S *Error;

int TYPEP(S *expr, S *type) {
  return expr->Type.type == type;
};

int LENGTH(S *expr) {
  int count;
  if(expr == nil) return 0;
  if (ATOMP(expr)) return 1;
  count = 1;
  while (!NILP(expr = REST(expr))) count++;
  return count;
}

static S *S_alloc() {
  return xmalloc(sizeof(S));
}

static struct Generic *Generics_alloc() {
  struct Generic *g;
  if ((g = (struct Generic *)malloc(sizeof(struct Generic))) == NULL) {
    fprintf(stderr, "Generics_alloc: Cannot allocate memory.");
    exit(1);
  }
  return g;
}

S *Cons_new(S *car, S *cdr) {
  S *prev;
  assert(!ATOMP(cdr) || NILP(cdr));
  prev = S_alloc();
  prev->Cons.type = Cons;
  FIRST(prev) = car;
  REST(prev) = cdr;
  return prev;
}

S *Symbol_new(char *name) {
  S *expr;
  expr = S_alloc();
  expr->Symbol.type = Symbol;
  expr->Symbol.name = name;
  return expr;
}

S *Keyword_new(char *val) {
  S *expr;
  if (strlen(val) == 0)
    return Error_new("Illegal Keyword.");
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

S *Char_new(char val) {
  S *expr;
  if (val == '\0')
    return Error_new("Illegal character.");
  expr = S_alloc();
  expr->Char.type = Char;
  expr->Char.val = val;
  return expr;
}

S *Number_new(double val) {
  S *expr;
  expr = S_alloc();
  expr->Number.type = Number;
  expr->Number.val = val;
  return expr;
}

// TODO: 総称関数のマッチ判定処理
static struct Generic *Function_lookupGenerics(S *fn, S *type) {
  struct Generic *generic;
  assert(TYPEP(fn, Function));
  return fn->Function.generics;
}

static void Function_addGenerics(S *fn, S *type, S *args, S *body, S *prim(S *)) {
  struct Generic *g;
  assert(TYPEP(fn, Function));
  if ((g = Function_lookupGenerics(fn, type)) == NULL)
    g = Generics_alloc();
  g->type = type;
  g->args = args;
  g->body = body;
  g->prim = prim;
  g->next = fn->Function.generics;
  fn->Function.generics = g;
}

S *Function_new(S *type, S *args, S *body, S *prim(S *)) {
  S *expr;
  expr = S_alloc();
  expr->Function.type = Function;
  expr->Function.generics = NULL;
  Function_addGenerics(expr, type, args, body, prim);
  return expr;
}

S *Special_new(S *fn(S *, Env *env)) {
  S *expr;
  expr = S_alloc();
  expr->Special.type = Special;
  expr->Special.fn = fn;
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

// nil? Object o
static S *Function_isNil(S *expr) {
  return NILP(expr)? t: nil;
}

static S *Function_isAtom(S *expr) {
  return ATOMP(expr)? t: nil;
}

static S *Function_car(S *expr) {
  if (LENGTH(expr) != 1)
    return Error_new("car: Illegal argument exception.");
  if (NILP(FIRST(expr)))
    return nil;
  if (FIRST(expr)->Cons.type != Cons)
    return Error_new("car: not a list.");
  return FIRST(FIRST(expr));
}

static S *Function_cdr(S *expr) {
  if (LENGTH(expr) != 1)
    return Error_new("cdr: Illegal argument exception.");
  if (NILP(FIRST(expr)))
    return nil;
  if (FIRST(expr)->Cons.type != Cons)
    return Error_new("cdr: not a list.");
  return REST(FIRST(expr));
}

static S *Function_cons(S *expr) {
  if (LENGTH(expr) != 2)
    return Error_new("cons: Illegal argument exception.");
  if (SECOND(expr)->Cons.type != Cons && !NILP(SECOND(expr)))
    return Error_new("cons: Illegal argument exception.");
  return Cons_new(FIRST(expr), SECOND(expr));
}

static S *Function_list(S *expr) {
  return expr;
}

static S *Function_length(S *expr) {
  if (NILP(expr))
    return Number_new(0);
  else if (TYPEP(expr, Cons))
    return Number_new(LENGTH(FIRST(expr)));
  else
    return Error_new("length: Cannot apply.");
}

// // TODO:
// static S *Function_desc(S *expr) {
//   printf("address: %d\n", (int)expr);
//   printf("type: %s\n", expr->Cons.type->Keyword.val);
//   switch (expr->Cons.type) {
//     case Cons:
//       printf("car: %d\n", (int)FIRST(expr));
//       printf("cdr: %d\n", (int)REST(expr));
//       break;
//     case Symbol:
//       printf("name: %s\n", expr->Symbol.name);
//       break;
//     case Keyword:
//       printf("name: %s\n", expr->Keyword.val);
//       break;
//     case String:
//       printf("value: %s\n", expr->String.val);
//       break;
//     case Char:
//       printf("value: %c\n", expr->Char.val);
//       break;
//     case Number:
//       printf("value: %f\n", expr->Number.val);
//       break;
//     case Function:
//       break;
//     case Stream:
//       break;
//     case Error:
//       break;
//     default:
//       break;
//   }
//   return expr;
// }

// special forms

static S *Special_ifElse(S *expr, Env *env) {
  if (LENGTH(expr) < 2)
    return Error_new("ifElse: Illegal argument exception.");
  while (!NILP(REST(expr))) {
    if (!NILP(S_eval(FIRST(expr), env)))
      return S_eval(SECOND(expr), env);
    if (NILP(expr = REST(expr)) || NILP(expr = REST(expr)))
      return nil;
  }
  return S_eval(FIRST(expr), env);
}

static S *Special_quote(S *expr, Env *env) {
  if (LENGTH(expr) != 1)
    return Error_new("quote: Illegal argument exception.");
  return FIRST(expr);
}

static S *Special_progn(S *expr, Env *env) {
  S *result;
  for (result = expr; !NILP(REST(result)); result = REST(result))
    S_eval(FIRST(result), env);
  return S_eval(FIRST(result), env);
}

static S *Special_let(S *expr, Env *env) {
  S *args, *cons, *result;
  if (LENGTH(expr) < 2 || ATOMP((args = FIRST(expr))) || (LENGTH(args) % 2) != 0)
    return Error_new("let: Illegal argument.");
  for (cons = args; !NILP(cons); cons = REST(REST(cons)))
    if (!TYPEP(FIRST(cons), Symbol))
      return Error_new("let: variable is not a symbol.");
  Env_push(env);
  for (cons = args; !NILP(cons); cons = REST(REST(cons)))
    Env_putSymbol(env, FIRST(cons)->Symbol.name, S_eval(SECOND(cons), env));
  result = Special_progn(REST(expr), env);
  Env_pop(env);
  return result;
}

static S *Special_assign(S *expr, Env *env) {
  S *cons, *var, *val;
  if (LENGTH(expr) < 2 || ATOMP(expr) || (LENGTH(expr) % 2) != 0)
    return Error_new("<-: Illegal argument.");
  for (cons = expr; !NILP(cons); cons = REST(REST(cons))) {
    if (!TYPEP(var = FIRST(cons), Symbol))
      return Error_new("<-: variable must be symbol.");
    if ((S *)Env_getSymbol(env, var->Symbol.name) == NULL)
      return Error_new("<-: undefined variable.");
  }
  for (cons = expr; !NILP(cons); cons = REST(REST(cons)))
    Env_putSymbol(env, FIRST(cons)->Symbol.name, val = S_eval(SECOND(cons), env));
  return val;
}

static S *Special_def(S *expr, Env *env) {
  S *cons, *var;
  for (cons = expr; !NILP(cons); cons = REST(cons)) {
    if (!TYPEP(var = FIRST(cons), Symbol))
      return Error_new("def: variable must be symbol.");
    if (Env_getSymbol(env, var->Symbol.name) != NULL)
      return Error_new("def: variable already defined.");
  }
  for (cons = expr; !NILP(cons); cons = REST(cons)) {
    Env_putSymbol(env, FIRST(cons)->Symbol.name, nil);
  }
  return nil;
}

static S *Special_fn(S *expr, Env *env) {
  S *args, *type;
  if (LENGTH(expr) <= 1 || ATOMP(args = FIRST(expr)))
    return Error_new("fn: Illegal argument.");
  if (TYPEP(type = FIRST(args), Keyword)) args = REST(args);
  else type = nil;
  return Function_new(type, args, REST(expr), NULL);
}

static S *S_errorHandler(S *expr) {
  S_print(expr);
  exit(1);
}

extern S *Lex_parseExpr();

S *S_read() {
  return Lex_parseExpr();
}

static S *S_apply(S *fn, S *args, Env *env) {
  struct Generic *generic;
  S *type, *fnArgs, *letArgs;
  type = FIRST(args)->Type.type;
  if ((generic = Function_lookupGenerics(fn, type)) == NULL)
    return Error_new("eval: method not found.");
  letArgs = nil;
  fnArgs = generic->args;
  while (!NILP(fnArgs)) {
    letArgs = Cons_new(FIRST(fnArgs), Cons_new(FIRST(args), letArgs));
    fnArgs = REST(fnArgs);
    args = REST(args);
  }
  return Special_let(Cons_new(letArgs, generic->body), env);
}

S *S_eval(S *expr, Env *env) {
  S *root, *fn, *args;
  // atom
  if (ATOMP(expr)) {
    if (TYPEP(expr, Symbol))
      if ((expr = Env_getSymbol(env, expr->Symbol.name)) == NULL)
        expr =  Error_new("eval: undefined variable.");
    return TYPEP(expr, Error)?
      S_errorHandler(expr):
      expr;
  }
  // special form
  else if ((fn = Env_getSpecial(env, FIRST(expr)->Symbol.name)) != NULL)
    return (fn->Special.fn)(REST(expr), env);
  // function
  else {
    root = expr;
    while (!NILP(expr)) {
      expr->Cons.car = S_eval(expr->Cons.car, env);
      expr = expr->Cons.cdr;
    }
    fn = FIRST(root);
    args = REST(root);
    if (fn->Function.type != Function) {
      return Error_new("eval: undefined function.");
    }
    return S_apply(fn, args, env);
  }
}

S *S_print(S *expr) {
  if (ATOMP(expr)) {
    if (TYPEP(expr, Number)) {
      double intptr, fraction;
      fraction = modf(expr->Number.val, &intptr);
      if (fraction == 0) printf("%d", (int)intptr);
      else printf("%f", expr->Number.val);
    }
    else if (TYPEP(expr, Char)) printf("%c", expr->Char.val);
    else if (TYPEP(expr, Function)) printf("<Function: %p>", expr);
    else if (TYPEP(expr, Keyword)) printf(":%s", expr->Keyword.val);
    else printf("%s", expr->String.val);
  }
  else {
    printf("(");
    S_print(expr->Cons.car);
    for (expr = expr->Cons.cdr; !NILP(expr); expr = expr->Cons.cdr) {
      if (ATOMP(expr->Cons.car)) printf(" ");
      S_print(expr->Cons.car);
    }
    printf(")");
  }
  fflush(stdout);
  return expr;
}

void Prim_init(Env *env) {

  // init ptimitive types
  Cons = Keyword_new("Cons");
  Symbol = Keyword_new("Symbol");
  Keyword = Keyword_new("Keyword");
  String = Keyword_new("String");
  Char = Keyword_new("Char");
  Number = Keyword_new("Number");
  Function = Keyword_new("Function");
  Special = Keyword_new("Special");
  Stream = Keyword_new("Stream");
  Error = Keyword_new("Error");

  // init special forms.
  Env_putSpecial(env, "ifElse", Special_new(Special_ifElse));
  Env_putSpecial(env, "quote", Special_new(Special_quote));
  Env_putSpecial(env, "progn", Special_new(Special_progn));
  Env_putSpecial(env, "let", Special_new(Special_let));
  Env_putSpecial(env, "<-", Special_new(Special_assign));
  Env_putSpecial(env, "def", Special_new(Special_def));
  Env_putSpecial(env, "fn", Special_new(Special_fn));

  // init global symbols.
  Env_putSymbol(env, "t", (t = Symbol_new("t")));
  Env_putSymbol(env, "nil", (nil = Symbol_new("nil")));
  Env_putSymbol(env, "stdin", Stream_new(stdin));
  Env_putSymbol(env, "stdout", Stream_new(stdout));
  Env_putSymbol(env, "stderr", Stream_new(stderr));
  Env_putSymbol(env, "pi", Number_new(3.14159265358979323846));

  // init functions.
  // Env_putSymbol(env, "nil?", Function_new(Function_isNil, NULL));
  // Env_putSymbol(env, "atom?", Function_new(Function_isAtom, NULL));
  // Env_putSymbol(env, "car", Function_new(Function_car, NULL));
  // Env_putSymbol(env, "cdr", Function_new(Function_cdr, NULL));
  // Env_putSymbol(env, "cons", Function_new(Function_cons, NULL));
  // Env_putSymbol(env, "list", Function_new(Function_list, NULL));
  // Env_putSymbol(env, "length", Function_new(Function_length, NULL));
  // Env_putSymbol(env, "desc", Function_new(Function_desc, NULL));
}
