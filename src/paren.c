/*
   paren main routine.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "std.h"
#include "splay.h"
#include "env.h"
#include "paren.h"
#include "reader.h"
#include "writer.h"

Env env;
static Reader rd;
static Writer wr;

extern S *Paren_eval(S *expr);

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
}

int LENGTH(S *expr) {
  int count;
  if(NILP(expr)) return 0;
  if (ATOMP(expr)) return 1;
  count = 1;
  while (!NILP(expr = REST(expr))) count++;
  return count;
}

static S *S_alloc() {
  return xmalloc(sizeof(S));
}

S *Cons_new(S *car, S *cdr) {
  S *root;
  assert(!ATOMP(cdr) || NILP(cdr));
  root = S_alloc();
  root->Cons.type = Cons;
  FIRST(root) = car;
  REST(root) = cdr;
  return root;
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

static void Function_addGeneric(S *fn, S *type, S *args, S *body, S *prim(S *))
{
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
  expr = S_alloc();
  expr->Function.type = Function;
  Function_addGeneric(expr, type, args, body, prim);
  return expr;
}

S *Special_new(S *fn(S *)) {
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
  expr->Stream.fp = stream;
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

// special forms

static S *Special_ifElse(S *expr) {
  if (LENGTH(expr) < 2)
    return Error_new("ifElse: Illegal argument exception.");
  while (!NILP(REST(expr))) {
    if (!NILP(Paren_eval(FIRST(expr))))
      return Paren_eval(SECOND(expr));
    if (NILP(expr = REST(expr)) || NILP(expr = REST(expr)))
      return nil;
  }
  return Paren_eval(FIRST(expr));
}

static S *Special_quote(S *expr) {
  int n;
  if ((n = LENGTH(expr)) > 1)
    return Error_new("quote: Illegal argument exception.");
  if (n == 0) return nil;    // '() => nil
  return FIRST(expr);
}

static S *Special_progn(S *expr) {
  S *result;
  while (!NILP(expr)) {
    result = Paren_eval(FIRST(expr));
    expr = REST(expr);
  }
  return result;
}

static S *Special_let(S *expr) {
  S *args, *cons, *result;
  if (!NILP(args = FIRST(expr)) && (ATOMP(args) || (LENGTH(args) % 2) != 0))
    return Error_new("let: Illegal argument.");
  for (cons = args; !NILP(cons); cons = REST(REST(cons)))
    if (!TYPEP(FIRST(cons), Symbol))
      return Error_new("let: variable is not a symbol.");
  if (LENGTH(expr) == 1) return nil;
  Env_push(&env);
  for (cons = args; !NILP(cons); cons = REST(REST(cons)))
    Env_putSymbol(&env, FIRST(cons)->Symbol.name
        , Paren_eval(SECOND(cons)));
  result = Special_progn(REST(expr));
  Env_pop(&env);
  return result;
}

static S *Special_assign(S *expr) {
  S *cons, *var, *val;
  if ((LENGTH(expr) % 2) != 0)
    return Error_new("<-: Illegal argument.");
  for (cons = expr; !NILP(cons); cons = REST(REST(cons))) {
    if (!TYPEP(var = FIRST(cons), Symbol))
      return Error_new("<-: variable must be symbol.");
    if (Env_getSymbol(&env, var->Symbol.name) == NULL)
      return Error_new("<-: undefined variable.");
  }
  for (cons = expr; !NILP(cons); cons = REST(REST(cons))) {
    S *envVal;
    if (TYPEP(val = Paren_eval(SECOND(cons)), Function) &&
        TYPEP(envVal = Env_getSymbol(&env, FIRST(cons)->Symbol.name), Function))
      val = Function_mergeGenerics(envVal, val);
    Env_putSymbol(&env, FIRST(cons)->Symbol.name, val);
  }
  return val;
}

static S *Special_def(S *expr) {
  S *cons, *var;
  for (cons = expr; !NILP(cons); cons = REST(cons)) {
    if (!TYPEP(var = FIRST(cons), Symbol))
      return Error_new("def: variable must be symbol.");
    if (Env_getSymbol(&env, var->Symbol.name) != NULL)
      return Error_new("def: variable already defined.");
  }
  for (cons = expr; !NILP(cons); cons = REST(cons))
    Env_putSymbol(&env, FIRST(cons)->Symbol.name, nil);
  return nil;
}

static S *Special_fn(S *expr) {
  S *args, *type;
  if (LENGTH(expr) <= 1 || ATOMP(args = FIRST(expr)))
    return Error_new("fn: Illegal argument.");
  if (TYPEP(type = FIRST(args), Keyword)) args = REST(args);
  else type = nil;
  return Function_new(type, args, REST(expr), NULL);
}

// primitive functions.

static S *Function_isNil(S *args) {
  if (LENGTH(args) != 1) return Error_new("nil?: illegal arguments");
  return NILP(FIRST(args))? t: nil;
}

static S *Function_isAtom(S *args) {
  if (LENGTH(args) != 1) return Error_new("nil?: illegal arguments");
  return ATOMP(FIRST(args))? t: nil;
}

static S *Function_car(S *args) {
  if (LENGTH(args) != 1) return Error_tooManyArgument("car", args);
  if (NILP(FIRST(args))) return nil;
  if (!TYPEP(FIRST(args), Cons)) return Error_new("car: not a list.");
  return FIRST(FIRST(args));
}

static S *Function_cdr(S *args) {
  if (LENGTH(args) != 1) return Error_new("cdr: Illegal argument exception.");
  if (NILP(FIRST(args))) return nil;
  if (!TYPEP(FIRST(args), Cons)) return Error_new("cdr: not a list.");
  return REST(FIRST(args));
}

static S *Function_cons(S *args) {
  if (LENGTH(args) != 2 || !(TYPEP(SECOND(args), Cons) || NILP(SECOND(args))))
    return Error_new("cons: Illegal argument exception.");
  return Cons_new(FIRST(args), SECOND(args));
}

static S *Function_list(S *args) {
  return args;
}

static S *Function_open(S *args) {
  FILE *fp;
  if (LENGTH(args) != 1)
    return Error_new("String.open: Illegal argument.");
  if ((fp = fopen(FIRST(args)->String.val, "r")) == NULL)
    return Error_new("String.open: cannnot open.");
  return Stream_new(fp);
}

static S *Function_close(S *args) {
  if (LENGTH(args) != 1)
    return Error_new("String.close: Illegal argument.");
  fclose(FIRST(args)->Stream.fp);
  FIRST(args)->Symbol.type = Symbol;
  FIRST(args)->Symbol.name = "nil";
  return nil;
}

static S *Function_getChar(S *expr) {
  int c;
  if (LENGTH(expr) != 1)
    return Error_new("Stream.getChar: Illegal argument.");
  c = fgetc(FIRST(expr)->Stream.fp);
  return c == EOF? nil: Char_new(c);
}

static S *Function_putChar(S *args) {
  S *c;
  if (LENGTH(args) != 2 || !TYPEP(c = SECOND(args), Char))
    return Error_new("Stream.putChar: Illegal argument.");
  fputc(c->Char.val, FIRST(args)->Stream.fp);
  return c;
}

static S *Function_Number_add(S *args) {
  S *acc;
  acc = Number_new(0);
  while (!NILP(args)) {
    if (!TYPEP(FIRST(args), Number))
      return Error_new("+: is not a number");
    acc->Number.val += FIRST(args)->Number.val;
    args = REST(args);
  }
  return acc;
}

static S *Function_Number_mul(S *args) {
  S *acc;
  acc = Number_new(1);
  while (!NILP(args)) {
    if (!TYPEP(FIRST(args), Number))
      return Error_new("*: is not a number");
    acc->Number.val *= FIRST(args)->Number.val;
    args = REST(args);
  }
  return acc;
}

static S *Function_read(S *args) {
  S *expr;
  FILE *fp;
  if (LENGTH(args) != 1) return Error_new("read: Illegal arguments.");
  fp = Reader_getFp(&rd);
  Reader_setFp(&rd, FIRST(args)->Stream.fp);
  expr = Reader_read(&rd);
  Reader_setFp(&rd, fp);
  return expr;
}

static S *Function_eval(S *args) {
  if (LENGTH(args) != 1) return Error_new("eval: Illegal arguments.");
  return Paren_eval(FIRST(args));
}

static S *Function_print(S *args) {
  FILE *fp;
  if (LENGTH(args) != 2) return Error_new("print: Illegal argument exception.");
  fp = Writer_getFp(&wr);    // backup
  Writer_setFp(&wr, FIRST(args)->Stream.fp);
  Writer_write(&wr, SECOND(args));
  Writer_setFp(&wr, fp);
  return SECOND(args);
}

static S *S_apply(S *fn, S *args) {
  struct Generic *generic;
  S *type, *fnArgs, *result;
  type = FIRST(args)->Type.type;
  if ((generic = Function_lookupGenerics(fn, type)) == NULL)
    return Error_new("eval: method not found.");
  // invoke primitive function.
  if (generic->args == NULL) return generic->prim(args);
  // invoke user defined function.
  fnArgs = generic->args;
  Env_push(&env);
  while (!NILP(fnArgs)) {
    Env_putSymbol(&env, FIRST(fnArgs)->Symbol.name, FIRST(args));
    fnArgs = REST(fnArgs);
    args = REST(args);
  }
  result = Special_progn(generic->body);
  Env_pop(&env);
  return result;
}

S *Paren_eval(S *expr) {
  S *root, *fn, *args;
  // atom
  if (ATOMP(expr)) {
    if (TYPEP(expr, Symbol))
      if ((expr = Env_getSymbol(&env, expr->Symbol.name)) == NULL)
        expr =  Error_new("eval: undefined variable.");
    return expr;
  }
  // special form
  if (TYPEP(FIRST(expr), Symbol)
      && (fn = Env_getSpecial(&env, FIRST(expr)->Symbol.name)) != NULL)
    return (fn->Special.fn)(REST(expr));
  // function
  if (LENGTH(expr) <= 1) return Error_new("eval: not found receiver.");
  root = expr;
  while (!NILP(expr)) {
    FIRST(expr) = Paren_eval(FIRST(expr));
    if (TYPEP(FIRST(expr), Error)) return FIRST(expr);
    expr = REST(expr);
  }
  fn = FIRST(root);
  args = REST(root);
  if (!TYPEP(fn, Function))
    return Error_new("eval: undefined function.");
  return S_apply(fn, args);
}

void Paren_init(Env *env, Reader *rd, Writer *wr) {

  setbuf(stdout, NULL);

  Env_init(env);
  Reader_init(rd, stdin);
  Writer_init(wr, stdout);

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
  Env_putSpecial(env, "<-", Special_new(Special_assign));
  Env_putSpecial(env, "def", Special_new(Special_def));
  Env_putSpecial(env, "fn", Special_new(Special_fn));
  Env_putSpecial(env, "ifElse", Special_new(Special_ifElse));
  Env_putSpecial(env, "let", Special_new(Special_let));
  Env_putSpecial(env, "progn", Special_new(Special_progn));
  Env_putSpecial(env, "quote", Special_new(Special_quote));

  // init functions.
  Env_putSymbol(env, "atom?", Function_new(nil, NULL, NULL, Function_isAtom));
  Env_putSymbol(env, "car", Function_new(nil, NULL, NULL, Function_car));
  Env_putSymbol(env, "cdr", Function_new(nil, NULL, NULL, Function_cdr));
  Env_putSymbol(env, "close", Function_new(Stream, NULL, NULL, Function_close));
  Env_putSymbol(env, "cons", Function_new(nil, NULL, NULL, Function_cons));
  Env_putSymbol(env, "eval", Function_new(nil, NULL, NULL, Function_eval));
  Env_putSymbol(env, "getChar" , Function_new(Stream, NULL, NULL, Function_getChar));
  Env_putSymbol(env, "list", Function_new(nil, NULL, NULL, Function_list));
  Env_putSymbol(env, "nil?", Function_new(nil, NULL, NULL, Function_isNil));
  Env_putSymbol(env, "open", Function_new(String, NULL, NULL, Function_open));
  Env_putSymbol(env, "print", Function_new(Stream, NULL, NULL, Function_print));
  Env_putSymbol(env, "putChar" , Function_new(Stream, NULL, NULL, Function_putChar));
  Env_putSymbol(env, "read", Function_new(Stream, NULL, NULL, Function_read));
  Env_putSymbol(env, "+", Function_new(Number, NULL, NULL, Function_Number_add));
  Env_putSymbol(env, "*", Function_new(Number, NULL, NULL, Function_Number_mul));
}

static void Paren_errorHandler(S *expr) {
  Writer_write(&wr, expr);
}

static void Paren_prompt() {
  printf(") ");
}

int main(int argc, char* argv[]) {
  void *expr;
  Paren_init(&env, &rd, &wr);
  Paren_prompt();
  while (1) {
    expr = Paren_eval(Reader_read(&rd));
    if (TYPEP(expr, Error)) Paren_errorHandler(expr);
    else Writer_write(&wr, expr);
    printf("\n");
    Paren_prompt();
  }
  return 0;
}
