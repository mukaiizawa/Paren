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
#include "lex.h"

// global symbols.

S *t;
S *nil;
S *quote;

// primitive types.

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

// global functions (for ast manipulation).

int TYPEP(S *expr, S *type) {
  assert(expr->Type.type->Keyword.type = Keyword);
  return expr->Type.type == type;
};

int LENGTH(S *expr) {
  int count;
  if(NILP(expr)) return 0;
  if (ATOMP(expr)) return 1;
  count = 1;
  while (!NILP(expr = REST(expr))) count++;
  return count;
}

// constructors.

static S *S_alloc() {
  return xmalloc(sizeof(S));
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

static struct Generic *Function_lookupGenerics(S *fn, S *type) {
  struct Generic *g;
  assert(TYPEP(fn, Function));
  for (g = fn->Function.generics; g != NULL; g = g->next)
    if (g->type == type) return g;
  for (g = fn->Function.generics; g != NULL; g = g->next)
    if (g->type == nil) return g;
  return NULL;
}

static void Function_addGeneric(S *fn, S *type, S *args, S *body, S *prim(S *)) {
  struct Generic *g;
  g = xmalloc(sizeof(struct Generic));
  g->type = type;
  g->args = args;
  g->body = body;
  g->prim = prim;
  g->next = fn->Function.generics;
  fn->Function.generics = g;
}

static S *Function_mergeGenerics(S *fnTo, S *fnFrom) {
  struct Generic *gTo, *gFrom;
  for (gFrom = fnFrom->Function.generics; gFrom != NULL; gFrom = gFrom->next) {
    gTo = Function_lookupGenerics(fnTo, gFrom->type);
    if (gTo == NULL || gTo->type == nil)
      Function_addGeneric(
          fnTo, gFrom->type, gFrom->args, gFrom->body, gFrom->prim);
    else {
      gTo->args = gFrom->args;
      gTo->body = gFrom->body;
      gTo->prim = gFrom->prim;
    }
  }
  return fnTo;
}

S *Function_new(S *type, S *args, S *body, S *prim(S *)) {
  S *expr;
  struct Generic *g;
  expr = S_alloc();
  expr->Function.type = Function;
  Function_addGeneric(expr, type, args, body, prim);
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

S *Error_tooManyArgument(char *name, S *body) {
  // char buf[MAX_STR_LEN], *sbody;
  // assert((strlen(name) + strlen(sbody = STRING(body))) < MAX_STR_LEN);
  // sprintf(buf, "eval: too many arguments (%s %s).", name, STRING(body));
  // return Error_new(buf);
  return Error_new("too many argument.");
}

// primitive functions.

static S *Function_isNil(S *expr) {
  return NILP(FIRST(expr))? t: nil;
}

static S *Function_isAtom(S *expr) {
  return ATOMP(expr)? t: nil;
}

static S *Function_car(S *expr) {
  if (LENGTH(expr) != 1) return Error_tooManyArgument("car", expr);
  if (NILP(FIRST(expr))) return nil;
  if (!TYPEP(FIRST(expr), Cons)) return Error_new("car: is not a list.");
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
  if (LENGTH(expr) != 2 || !(TYPEP(SECOND(expr), Cons) || NILP(SECOND(expr))))
    return Error_new("cons: Illegal argument exception.");
  return Cons_new(FIRST(expr), SECOND(expr));
}

// defined as primitive function because of performance.
static S *Function_list(S *expr) {
  return expr;
}

static S *Function_open(S *expr) {
  FILE *fp;
  if (LENGTH(expr) != 1)
    return Error_new("String.open: Illegal argument.");
  if ((fp = fopen(FIRST(expr)->String.val, "r")) == NULL)
    return Error_new("String.open: cannnot open.");
  return Stream_new(fp);
}

static S *Function_close(S *expr) {
  FILE *fp;
  if (LENGTH(expr) != 1)
    return Error_new("String.close: Illegal argument.");
  fclose(FIRST(expr)->Stream.stream);
  FIRST(expr)->Symbol.type = Symbol;
  FIRST(expr)->Symbol.name = "nil";
  return nil;
}

static S *Function_getChar(S *expr) {
  int c;
  if (LENGTH(expr) != 1)
    return Error_new("Stream.getChar: Illegal argument.");
  c = fgetc(FIRST(expr)->Stream.stream);
  return c == EOF? nil: Char_new(c);
}

static S *Function_putChar(S *expr) {
  S *stream, *c;
  if (LENGTH(expr) != 2 || !TYPEP(c = SECOND(expr), Char))
    return Error_new("Stream.putChar: Illegal argument.");
  fputc(c->Char.val, FIRST(expr)->Stream.stream);
  return c;
}

static S *Function_Cons_asString(S *expr) {
  return String_new("");
}

static S *Function_Symbol_asString(S *expr) {
  return String_new(FIRST(expr)->Symbol.name);
}

static S *Function_Keyword_asString(S *expr) {
  return String_new(FIRST(expr)->Keyword.val);
}

static S *Function_String_asString(S *expr) {
  return String_new(FIRST(expr)->String.val);
}

static S *Function_Char_asString(S *expr) {
  char *s;
  s = xmalloc(sizeof(char) * 2);
  s[0] = FIRST(expr)->Char.val;
  s[1] = '\0';
  return String_new(s);
}

static S *Function_Number_asString(S *expr) {
  char *str;
  str = xmalloc(MAX_STR_LEN);
  double intptr, fraction;
  fraction = modf(FIRST(expr)->Number.val, &intptr);
  if (fraction == 0) xsnprintf(str, "%d", (int)intptr);
  else xsnprintf(str, "%f", FIRST(expr)->Number.val);
  return String_new(str);
}

static S *Function_Function_asString(S *expr) {
  char *str;
  str = xmalloc(MAX_STR_LEN);
  xsnprintf(str, "<Function: %p>", FIRST(expr));
  return String_new(str);
}

static S *Function_Stream_asString(S *expr) {
  char *str;
  str = xmalloc(MAX_STR_LEN);
  xsnprintf(str, "<Stream: %p>", FIRST(expr));
  return String_new(str);
}

static S *Function_Error_asString(S *expr) {
  return String_new(FIRST(expr)->Error.val);
}

static S *Function_String_desc(S *expr) {
  printf("<Type: String, Value: %s, Address: %p>\n"
      , expr->String.val
      , expr);
  return expr;
}

static S *Function_Char_desc(S *expr) {
  printf("<Type: Char, Value: %c, Address: %p>\n"
      , expr->Char.val
      , expr);
  return expr;
}

static S *Function_Keyword_desc(S *expr) {
  printf("<Type: Keyword, Value: %s, Address: %p>\n"
      , expr->Keyword.val
      , expr);
  return expr;
}

static S *Function_Function_desc(S *expr) {
  struct Generic *g;
  printf("<Type: Function, Address: %p\n", expr);
  printf("Generics:\n");
  for (g = expr->Function.generics; g != NULL; g = g->next) {
    printf("\tType: %s\n", g->type->Keyword.val);
  }
  printf(">\n");
  return expr;
}

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
  int n;
  if ((n = LENGTH(expr)) > 1)
    return Error_new("quote: Illegal argument exception.");
  if (n == 0) return nil;    // '() => nil
  return FIRST(expr);
}

static S *Special_progn(S *expr, Env *env) {
  S *result;
  while (!NILP(expr)) {
    result = S_eval(FIRST(expr), env);
    expr = REST(expr);
  }
  return result;
}

static S *Special_let(S *expr, Env *env) {
  S *args, *cons, *result;
  if (!NILP(args = FIRST(expr)) && (ATOMP(args) || (LENGTH(args) % 2) != 0))
    return Error_new("let: Illegal argument.");
  for (cons = args; !NILP(cons); cons = REST(REST(cons)))
    if (!TYPEP(FIRST(cons), Symbol))
      return Error_new("let: variable is not a symbol.");
  if (LENGTH(expr) == 1) return nil;
  Env_push(env);
  for (cons = args; !NILP(cons); cons = REST(REST(cons)))
    Env_putSymbol(env, FIRST(cons)->Symbol.name, S_eval(SECOND(cons), env));
  result = Special_progn(REST(expr), env);
  Env_pop(env);
  return result;
}

static S *Special_assign(S *expr, Env *env) {
  S *cons, *var, *val;
  if ((LENGTH(expr) % 2) != 0)
    return Error_new("<-: Illegal argument.");
  for (cons = expr; !NILP(cons); cons = REST(REST(cons))) {
    if (!TYPEP(var = FIRST(cons), Symbol))
      return Error_new("<-: variable must be symbol.");
    if (Env_getSymbol(env, var->Symbol.name) == NULL)
      return Error_new("<-: undefined variable.");
  }
  for (cons = expr; !NILP(cons); cons = REST(REST(cons))) {
    S *envVal;
    if (TYPEP(val = S_eval(SECOND(cons), env), Function) &&
        TYPEP(envVal = Env_getSymbol(env, FIRST(cons)->Symbol.name), Function))
      val = Function_mergeGenerics(envVal, val);
    Env_putSymbol(env, FIRST(cons)->Symbol.name, val);
  }
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
  for (cons = expr; !NILP(cons); cons = REST(cons))
    Env_putSymbol(env, FIRST(cons)->Symbol.name, nil);
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

S *S_read(Env *env, FILE *fp) {
  Lex_init(env, fp);
  return Lex_parseExpr();
}

static S *S_apply(S *fn, S *args, Env *env) {
  struct Generic *generic;
  S *type, *fnArgs, *result;
  type = FIRST(args)->Type.type;
  if ((generic = Function_lookupGenerics(fn, type)) == NULL)
    return Error_new("eval: method not found.");
  // invoke primitive function.
  if (generic->args == NULL) return generic->prim(args);
  // invoke user defined function.
  fnArgs = generic->args;
  Env_push(env);
  while (!NILP(fnArgs)) {
    Env_putSymbol(env, FIRST(fnArgs)->Symbol.name, FIRST(args));
    fnArgs = REST(fnArgs);
    args = REST(args);
  }
  result = Special_progn(generic->body, env);
  Env_pop(env);
  return result;
}

S *S_eval(S *expr, Env *env) {
  S *root, *fn, *args;
  // atom
  if (ATOMP(expr)) {
    if (TYPEP(expr, Symbol))
      if ((expr = Env_getSymbol(env, expr->Symbol.name)) == NULL)
        expr =  Error_new("eval: undefined variable.");
    return expr;
  }
  // special form
  if (TYPEP(FIRST(expr), Symbol)
      && (fn = Env_getSpecial(env, FIRST(expr)->Symbol.name)) != NULL)
    return (fn->Special.fn)(REST(expr), env);
  // function
  if (LENGTH(expr) <= 1) return Error_new("eval: not found receiver.");
  root = expr;
  while (!NILP(expr)) {
    FIRST(expr) = S_eval(expr->Cons.car, env);
    if (TYPEP(FIRST(expr), Error)) return FIRST(expr);
    expr = expr->Cons.cdr;
  }
  fn = FIRST(root);
  args = REST(root);
  if (fn->Function.type != Function)
    return Error_new("eval: undefined function.");
  return S_apply(fn, args, env);
}

S *S_print(S *expr) {
  if (ATOMP(expr)) {
    if (TYPEP(expr, Number)) {
      double intptr, fraction;
      fraction = modf(expr->Number.val, &intptr);
      if (fraction == 0) printf("%d", (int)intptr);
      else printf("%f", expr->Number.val);
    }
    else if (TYPEP(expr, Symbol)) printf("%s", expr->Symbol.name);
    else if (TYPEP(expr, Function)) printf("<Function: %p>", expr);
    else if (TYPEP(expr, Keyword)) printf(":%s", expr->Keyword.val);
    else if (TYPEP(expr, Char)) printf("%c", expr->Char.val);
    else if (TYPEP(expr, Stream)) printf("<Stream: %p>", expr);
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
  Keyword = Keyword_new("Keyword");
  Keyword->Keyword.type = Keyword;
  Env_putKeyword(env, "Cons", Cons = Keyword_new("Cons"));
  Env_putKeyword(env, "Symbol", Symbol = Keyword_new("Symbol"));
  Env_putKeyword(env, "Keyword", Keyword);
  Env_putKeyword(env, "String", String = Keyword_new("String"));
  Env_putKeyword(env, "Char", Char = Keyword_new("Char"));
  Env_putKeyword(env, "Number", Number = Keyword_new("Number"));
  Env_putKeyword(env, "Function", Function = Keyword_new("Function"));
  Env_putKeyword(env, "Special", Special = Keyword_new("Special"));
  Env_putKeyword(env, "Stream", Stream = Keyword_new("Stream"));
  Env_putKeyword(env, "Error", Error = Keyword_new("Error"));

  // init global symbols.
  quote = Symbol_new("quote");
  Env_putSymbol(env, "t", t = Symbol_new("t"));
  Env_putSymbol(env, "nil", nil = Symbol_new("nil"));
  Env_putSymbol(env, "stdin", Stream_new(stdin));
  Env_putSymbol(env, "stdout", Stream_new(stdout));
  Env_putSymbol(env, "stderr", Stream_new(stderr));
  Env_putSymbol(env, "pi", Number_new(3.14159265358979323846));

  // init special forms.
  Env_putSpecial(env, "ifElse", Special_new(Special_ifElse));
  Env_putSpecial(env, "quote", Special_new(Special_quote));
  Env_putSpecial(env, "progn", Special_new(Special_progn));
  Env_putSpecial(env, "let", Special_new(Special_let));
  Env_putSpecial(env, "<-", Special_new(Special_assign));
  Env_putSpecial(env, "def", Special_new(Special_def));
  Env_putSpecial(env, "fn", Special_new(Special_fn));

  // init functions.
  Env_putSymbol(env, "nil?", Function_new(nil, NULL, NULL, Function_isNil));
  Env_putSymbol(env, "atom?", Function_new(nil, NULL, NULL, Function_isAtom));
  Env_putSymbol(env, "car", Function_new(nil, NULL, NULL, Function_car));
  Env_putSymbol(env, "cdr", Function_new(nil, NULL, NULL, Function_cdr));
  Env_putSymbol(env, "cons", Function_new(nil, NULL, NULL, Function_cons));
  Env_putSymbol(env, "list", Function_new(nil, NULL, NULL, Function_list));
  Env_putSymbol(env, "open", Function_new(String, NULL, NULL, Function_open));
  Env_putSymbol(env, "close", Function_new(Stream, NULL, NULL, Function_close));
  Env_putSymbol(env, "getChar", Function_new(Stream, NULL, NULL, Function_getChar));
  Env_putSymbol(env, "putChar", Function_new(Stream, NULL, NULL, Function_putChar));
  Env_putSymbol(env, "asString", 
      Function_mergeGenerics(
        Function_mergeGenerics(
          Function_mergeGenerics(
            Function_mergeGenerics(
              Function_mergeGenerics(
                Function_mergeGenerics(
                  Function_mergeGenerics(
                    Function_mergeGenerics(
                      Function_new(Cons, NULL, NULL, Function_Cons_asString)
                      , Function_new(Symbol, NULL, NULL, Function_Symbol_asString))
                    , Function_new(Keyword, NULL, NULL, Function_Keyword_asString))
                  , Function_new(String, NULL, NULL, Function_String_asString))
                , Function_new(Char, NULL, NULL, Function_Char_asString))
              , Function_new(Number, NULL, NULL, Function_Number_asString))
            , Function_new(Function, NULL, NULL, Function_Function_asString))
          , Function_new(Stream, NULL, NULL, Function_Stream_asString))
        , Function_new(Error, NULL, NULL, Function_Error_asString)));
}
