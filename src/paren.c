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
#include "gc.h"
#include "reader.h"
#include "writer.h"

Env env;
static Reader rd;
static Writer wr;
static S *consPool;

extern S *EVAL(S *expr);

// global symbols.

S *t;
S *nil;
S *eof;
S *quote;

// primitive types.

S *Cons;
S *Symbol;
S *Keyword;
S *String;
S *Char;
S *Number;
S *Special;
S *Macro;
S *Function;
S *Stream;
S *Error;

// global functions (for ast manipulation).

int TYPEP(S *expr, S *type) {
  assert(expr->Object.type->Keyword.type = Keyword);
  return expr->Object.type == type;
}

int LENGTH(S *expr) {
  int count;
  if(NILP(expr)) return 0;
  if (ATOMP(expr)) return 1;
  count = 1;
  while (!NILP(expr = REST(expr))) count++;
  return count;
}

S *REVERSE(S *expr) {
  S *root, *cdr;
  assert(NILP(expr) || TYPEP(expr, Cons));
  root = nil;
  while (!NILP(expr)) {
    cdr = root;
    root = expr;
    expr = REST(expr);
    REST(root) = cdr;
  }
  return root;
}

static int EQ(S *arg1, S *arg2) {
  S *type;
  if ((type = arg1->Object.type) != arg2->Object.type) return 0;
  else if (type == String)
    return strcmp(arg1->String.val, arg2->String.val) == 0;
  else if (type == Symbol)
    return strcmp(arg1->Symbol.name, arg2->Symbol.name) == 0;
  else if (type == Char) return arg1->Char.val == arg2->Char.val;
  else if (type == Number) return arg1->Number.val == arg2->Number.val;
  else return arg1 == arg2;
}

static S *S_alloc() {
  S *expr;
  expr = xmalloc(sizeof(S));
  expr->Object.age = GC_NEWBIE;
  return expr;
}

S *Cons_new(S *car, S *cdr) {
  S *root;
  assert(!ATOMP(cdr) || NILP(cdr));
  if (!NILP(REST(consPool))) {
    root = REST(consPool);
    REST(consPool) = REST(root);
  }
  else {
    root = S_alloc();
    root->Cons.type = Cons;
    root->Keyword.age = GC_PERM;
  }
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
  if (strlen(val) == 0) return Error_new("read: Illegal Keyword.");
  if ((expr = Env_getKeyword(&env, val)) != NULL) {
    free(val);
    return expr;
  }
  expr = S_alloc();
  expr->Keyword.type = Keyword;
  expr->Keyword.age = GC_PERM;
  expr->Keyword.val = val;
  Env_putKeyword(&env, val, expr);
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
  if (val == '\0') return Error_new("read: Illegal character.");
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

static struct Generic *Function_lookup(S *fn, S *type) {
  struct Generic *g;
  assert(TYPEP(fn, Function));
  for (g = fn->Function.generics; g != NULL; g = g->next)
    if (g->type == type) return g;
  return fn->Function.gDefault;
}

static void Function_addGeneric(
    S *fn, S *type, S *params, S *body , S *prim(S *)) {
  struct Generic *g;
  g = xmalloc(sizeof(struct Generic));
  g->type = type;
  g->params = params;
  g->body = body;
  g->prim = prim;
  if (type == nil) {
    g->next = NULL;
    fn->Function.gDefault = g;
  }
  else {
    g->next = fn->Function.generics;
    fn->Function.generics = g;
  }
}

static S *Function_merge(S *fnTo, S *fnFrom) {
  struct Generic *gFrom, *gTo;
  assert(TYPEP(fnTo, Function) && TYPEP(fnFrom, Function));
  for (gFrom = fnFrom->Function.generics; gFrom != NULL; gFrom = gFrom->next) {
    gTo = Function_lookup(fnTo, gFrom->type);
    if (gTo != NULL && gTo->type != nil) {
      gTo->params = gFrom->params;
      gTo->body = gFrom->body;
      gTo->prim = gFrom->prim;
    }
    else
      Function_addGeneric(
          fnTo, gFrom->type, gFrom->params, gFrom->body, gFrom->prim);
  }
  if ((gFrom = fnFrom->Function.gDefault) != NULL) {
    gTo = fnTo->Function.gDefault;
    if (gTo == NULL) gTo = xmalloc(sizeof(struct Generic));
    gTo->params = gFrom->params;
    gTo->body = gFrom->body;
    gTo->prim = gFrom->prim;
  }
  return fnTo;
}

S *Special_new(S *fn(S *)) {
  S *expr;
  expr = S_alloc();
  expr->Special.type = Special;
  expr->Special.age = GC_PERM;
  expr->Special.fn = fn;
  return expr;
}

S *Macro_new(S *params, S *body) {
  S *expr;
  expr = S_alloc();
  expr->Macro.type = Macro;
  expr->Macro.age = GC_PERM;
  expr->Macro.params = params;
  expr->Macro.body = body;
  return expr;
}

S *Function_new(S *type, S *params, S *body, S *prim(S *)) {
  S *expr;
  expr = S_alloc();
  expr->Function.type = Function;
  expr->Function.gDefault = expr->Function.generics = NULL;
  Function_addGeneric(expr, type, params, body, prim);
  return expr;
}

S *Stream_new(FILE *stream) {
  S *expr;
  expr = S_alloc();
  expr->Stream.type = Stream;
  expr->Stream.fp = stream;
  return expr;
}

S *Error_new(char *val) {
  S *expr;
  expr = S_alloc();
  expr->Error.type = Error;
  expr->Error.val = val;
  return expr;
}

S *Error_illegalArgument(char *name, int provided, int min, int max) {
  S *err;
  char *s1, *s2, *s3, *s4;
  if (provided < min) {
    s1 = "few";
    s2 = "least ";
    s3 = xitoa(min);
    s4 = " required.";
  }
  else {
    s1 = "many";
    s2 = "most ";
    s3 = xitoa(max);
    s4 = " accepted.";
  }
  err = Error_new(
      xvstrcat("eval: Too ", s1, "arguments in call to `", name, "' "
        , xitoa(provided), " arguments provided, at ", s2, s3, s4));
  free(s3);
  return err;
}

void Cons_free(S *expr) {
  S_free(expr->Cons.car);
  S_free(expr->Cons.cdr);
  REST(expr) = REST(consPool);
  REST(consPool) = expr;
}

void Symbol_free(S *expr) {
  free(expr->Symbol.name);
  free(expr);
}

void String_free(S *expr) {
  free(expr->String.val);
  free(expr);
}

void Macro_free(S *expr) {
  S_free(expr->Macro.params);
  S_free(expr->Macro.body);
  free(expr);
}

void Function_free(S *expr) {
  struct Generic *g;
  while (expr->Function.generics != NULL) {
    g = expr->Function.generics;
    expr->Function.generics = g->next;
    free(g);
  }
  free(expr->Function.gDefault);
  free(expr);
}

void Error_free(S *expr) {
  free(expr->Error.val);
  free(expr);
}

void S_free(S *expr) {
  if (TYPEP(expr, Cons)) return Cons_free(expr);
  else if (TYPEP(expr, Symbol)) return Symbol_free(expr);
  else if (TYPEP(expr, Keyword) || TYPEP(expr, Special)) return;
  else if (TYPEP(expr, String)) return String_free(expr);
  else if (TYPEP(expr, Macro)) return Macro_free(expr);
  else if (TYPEP(expr, Function)) return Function_free(expr);
  else if (TYPEP(expr, Error)) return Error_free(expr);
  else free(expr);
}

// special forms

static S *Special_ifElse(S *expr) {
  int len;
  if ((len = LENGTH(expr)) < 2)
    return Error_illegalArgument("ifElse", len, 2, -1);
  while (!NILP(REST(expr))) {
    if (!NILP(EVAL(FIRST(expr)))) return EVAL(SECOND(expr));
    if (NILP(expr = REST(expr)) || NILP(expr = REST(expr))) return nil;
  }
  return EVAL(FIRST(expr));
}

static S *Special_quote(S *expr) {
  int len;
  if ((len = LENGTH(expr)) > 1)
    return Error_illegalArgument("quote", len, 1, 1);
  if (len == 0) return nil;    // `() => nil
  return FIRST(expr);
}

static S *Special_progn(S *expr) {
  S *result;
  while (!NILP(expr)) {
    if (TYPEP(result = EVAL(FIRST(expr)), Error)) return result;
    expr = REST(expr);
  }
  return result;
}

static S *Special_let(S *expr) {
  S *params, *cons, *result;
  if (NILP(params = FIRST(expr))) return Special_progn(REST(expr));
  if (ATOMP(params) || (LENGTH(params) % 2) != 0)
    return Error_new("let: Illegal parameter.");
  for (cons = params; !NILP(cons); cons = REST(REST(cons)))
    if (!TYPEP(FIRST(cons), Symbol))
      return Error_new("let: Parameter must be symbol.");
  if (LENGTH(expr) == 1) return nil;    // (let ())
  Env_push(&env);
  for (cons = params; !NILP(cons); cons = REST(REST(cons)))
    Env_putSymbol(&env, FIRST(cons)->Symbol.name , EVAL(SECOND(cons)));
  result = Special_progn(REST(expr));
  Env_pop(&env);
  return result;
}

static S *Special_assign(S *expr) {
  S *cons, *var, *val;
  if ((LENGTH(expr) % 2) != 0) return Error_new("<-: Illegal argument.");
  for (cons = expr; !NILP(cons); cons = REST(REST(cons))) {
    if (!TYPEP(var = FIRST(cons), Symbol))
      return Error_new("<-: variable must be symbol.");
    if (Env_getSymbol(&env, var->Symbol.name) == NULL)
      return Error_new("<-: undefined variable.");
  }
  for (cons = expr; !NILP(cons); cons = REST(REST(cons))) {
    S *envVal;
    if (TYPEP(val = EVAL(SECOND(cons)), Error)) return val;
    if (TYPEP(val, Function) &&
        TYPEP(envVal = Env_getSymbol(&env, FIRST(cons)->Symbol.name), Function))
      val = Function_merge(envVal, val);
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

static S *Special_macro(S *expr) {
  if (LENGTH(expr) < 1 || ATOMP(FIRST(expr)))
    return Error_new("macro: Illegal argument.");
  return Macro_new(FIRST(expr), REST(expr));
}

static S *Special_fn(S *expr) {
  S *params, *type;
  if (LENGTH(expr) < 1 || ATOMP(params = FIRST(expr)))
    return Error_new("fn: Illegal argument.");
  if (TYPEP(type = FIRST(params), Keyword)) params = REST(params);
  else type = nil;
  return Function_new(type, params, REST(expr), NULL);
}

// primitive functions.

static S *Function_isEqual(S *args) {
  int len;
  if ((len = LENGTH(args)) < 2) return Error_illegalArgument("=?", len, 2, -1);
  while (!NILP(REST(args))) {
    if (!EQ(FIRST(args), SECOND(args))) return nil;
    args = REST(args);
  }
  return t;
}

static S *Function_isNil(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument("nil?", len, 1, 1);
  return NILP(FIRST(args))? t: nil;
}

static S *Function_isAtom(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument("atom?", len, 1, 1);
  return ATOMP(FIRST(args))? t: nil;
}

static S *Function_car(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1) return Error_illegalArgument("car", len, 1, 1);
  if (NILP(FIRST(args))) return nil;
  if (!TYPEP(FIRST(args), Cons)) return Error_new("car: Not a list.");
  return FIRST(FIRST(args));
}

static S *Function_cdr(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1) return Error_illegalArgument("cdr", len, 1, 1);
  if (NILP(FIRST(args))) return nil;
  if (!TYPEP(FIRST(args), Cons)) return Error_new("cdr: Not a list.");
  return REST(FIRST(args));
}

static S *Function_cons(S *args) {
  int len;
  if ((len = LENGTH(args)) != 2)
    return Error_illegalArgument("cons", len, 2, 2);
  if (!TYPEP(SECOND(args), Cons) && !NILP(SECOND(args)))
    return Error_new("cons: Must be terminated with nil.");
  return Cons_new(FIRST(args), SECOND(args));
}

static S *Function_open(S *args) {
  int len;
  FILE *fp;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument("open", len, 1, 1);
  if ((fp = fopen(FIRST(args)->String.val, "r")) == NULL)
    return Error_new("String.open: cannnot open.");
  return Stream_new(fp);
}

static S *Function_close(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument("close", len, 1, 1);
  fclose(FIRST(args)->Stream.fp);
  FIRST(args)->Symbol.type = Symbol;
  FIRST(args)->Symbol.name = "nil";
  return nil;
}

static S *Function_getChar(S *args) {
  int len, c;
  if ((len = LENGTH(args)) > 2)
    return Error_illegalArgument("getChar", len, 1, 2);
  c = fgetc(FIRST(args)->Stream.fp);
  if (c != EOF) return Char_new(c);
  else if (len == 2) return SECOND(args);
  else return eof;
}

static S *Function_putChar(S *args) {
  int len;
  S *c;
  if ((len = LENGTH(args)) != 2)
    return Error_illegalArgument("putChar", len, 2, 2);
  if (!TYPEP(c = SECOND(args), Char))
    return Error_new("putChar: Second argument must be character.");
  fputc(c->Char.val, FIRST(args)->Stream.fp);
  return c;
}

static S *Function_Number_add(S *args) {
  S *acc;
  acc = Number_new(0);
  while (!NILP(args)) {
    if (!TYPEP(FIRST(args), Number)) return Error_new("+: is not a number");
    acc->Number.val += FIRST(args)->Number.val;
    args = REST(args);
  }
  return acc;
}

static S *Function_Number_mul(S *args) {
  S *acc;
  acc = Number_new(1);
  while (!NILP(args)) {
    if (!TYPEP(FIRST(args), Number)) return Error_new("*: is not a number");
    acc->Number.val *= FIRST(args)->Number.val;
    args = REST(args);
  }
  return acc;
}

static S *Function_read(S *args) {
  int len;
  Reader rd;
  if ((len = LENGTH(args)) > 2)
    return Error_illegalArgument("read", len, 1, 2);
  Reader_init(&rd, FIRST(args)->Stream.fp, ((len == 2)? SECOND(args): eof));
  return Reader_read(&rd);
}

static S *Function_eval(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument("eval", len, 1, 1);
  return EVAL(FIRST(args));
}

static S *Function_print(S *args) {
  int len;
  Writer wr;
  if ((len = LENGTH(args)) != 2)
    return Error_illegalArgument("print", len, 2, 2);
  Writer_init(&wr, FIRST(args)->Stream.fp);
  Writer_write(&wr, SECOND(args));
  return SECOND(args);
}

static S *APPLY(S *params, S *body, S *args) {
  S *result;
  Env_push(&env);
  while (!NILP(params)) {
    Env_putSymbol(&env, FIRST(params)->Symbol.name, FIRST(args));
    params = REST(params);
    args = REST(args);
  }
  result = Special_progn(body);
  Env_pop(&env);
  return result;
}

S *EVAL(S *expr) {
  S *fn, *acc;
  // atom
  if (ATOMP(expr)) {
    if (!TYPEP(expr, Symbol)) return expr;
    if ((acc = Env_getSymbol(&env, expr->Symbol.name)) != NULL) return acc;
    return Error_new(
        xvstrcat("eval: undefined variable `", expr->Symbol.name, "'."));
  }
  if (TYPEP(FIRST(expr), Symbol)) {
    // macro
    if ((fn = Env_getSymbol(&env, FIRST(expr)->Symbol.name)) != NULL
        && TYPEP(fn, Macro))
      return EVAL(APPLY(fn->Macro.params, fn->Macro.body, REST(expr)));
    // special form
    if ((fn = Env_getSpecial(&env, FIRST(expr)->Symbol.name)) != NULL)
      return (fn->Special.fn)(REST(expr));
  }
  // function
  if (LENGTH(expr) <= 1) return Error_new("eval: not found receiver.");
  acc = nil;
  while (!NILP(expr)) {
    acc = Cons_new(EVAL(FIRST(expr)), acc);
    if (TYPEP(FIRST(acc), Error)) return FIRST(acc);
    expr = REST(expr);
  }
  acc = REVERSE(acc);
  fn = FIRST(acc);
  if (!TYPEP(fn, Function)) return Error_new("eval: Undefined function.");
  else {
    struct Generic *g;
    if ((g = Function_lookup(fn, SECOND(acc)->Object.type)) == NULL)
      return Error_new("eval: Method not found.");
    // invoke primitive function.
    if (g->params == NULL) return g->prim(REST(acc));
    // invoke user defined function.
    else return APPLY(g->params, g->body, REST(acc));
  }
}

void Paren_init(Env *env, Reader *rd, Writer *wr) {

  setbuf(stdout, NULL);

  Env_init(env);

  // init ptimitive types.
  Keyword = Keyword_new("Keyword");
  Keyword->Keyword.type = Keyword;
  Env_putKeyword(env, "t", t = Keyword_new("t"));
  Env_putKeyword(env, "nil", nil = Keyword_new("nil"));
  Env_putKeyword(env, "EOF", eof = Keyword_new("EOF"));
  Env_putKeyword(env, "Cons", Cons = Keyword_new("Cons"));
  Env_putKeyword(env, "Symbol", Symbol = Keyword_new("Symbol"));
  Env_putKeyword(env, "Keyword", Keyword);
  Env_putKeyword(env, "String", String = Keyword_new("String"));
  Env_putKeyword(env, "Char", Char = Keyword_new("Char"));
  Env_putKeyword(env, "Number", Number = Keyword_new("Number"));
  Env_putKeyword(env, "Special", Special = Keyword_new("Special"));
  Env_putKeyword(env, "Macro", Macro = Keyword_new("Macro"));
  Env_putKeyword(env, "Function", Function = Keyword_new("Function"));
  Env_putKeyword(env, "Stream", Stream = Keyword_new("Stream"));
  Env_putKeyword(env, "Error", Error = Keyword_new("Error"));

  // init cons pool.
  consPool = S_alloc();
  consPool->Cons.type = Cons;
  FIRST(consPool) = nil;
  REST(consPool) = nil;

  // init global symbols.
  quote = Symbol_new("quote");
  Env_putSymbol(env, "stdin", Stream_new(stdin));
  Env_putSymbol(env, "stdout", Stream_new(stdout));
  Env_putSymbol(env, "stderr", Stream_new(stderr));
  Env_putSymbol(env, "pi", Number_new(3.14159265358979323846));

  // init special forms.
  Env_putSpecial(env, "<-", Special_new(Special_assign));
  Env_putSpecial(env, "def", Special_new(Special_def));
  Env_putSpecial(env, "macro", Special_new(Special_macro));
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
  Env_putSymbol(env, "nil?", Function_new(nil, NULL, NULL, Function_isNil));
  Env_putSymbol(env, "open", Function_new(String, NULL, NULL, Function_open));
  Env_putSymbol(env, "print", Function_new(Stream, NULL, NULL, Function_print));
  Env_putSymbol(env, "putChar", Function_new(Stream, NULL, NULL, Function_putChar));
  Env_putSymbol(env, "read", Function_new(Stream, NULL, NULL, Function_read));
  Env_putSymbol(env, "+", Function_new(Number, NULL, NULL, Function_Number_add));
  Env_putSymbol(env, "*", Function_new(Number, NULL, NULL, Function_Number_mul));
  Env_putSymbol(env, "=?", Function_new(nil, NULL, NULL, Function_isEqual));

  // init reader and writer.
  Reader_init(rd, stdin, eof);
  Writer_init(wr, stdout);

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
  while ((expr = Reader_read(&rd)) != eof) {
    expr = EVAL(expr);
    if (TYPEP(expr, Error)) Paren_errorHandler(expr);
    else Writer_write(&wr, expr);
    printf("\n");
    Paren_prompt();
  }
  return 0;
}
