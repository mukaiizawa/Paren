/*
   paren main routine.
*/

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
static S *pool;

extern S *EVAL(S *expr);
extern S *APPLY(S *fn, S *args);

// global symbols.
S *t;
S *nil;
S *eof;
S *quote;
S *dot;

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
  if (NILP(expr)) return 0;
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

int EQ(S *arg1, S *arg2) {
  S *type;
  if ((type = arg1->Object.type) != arg2->Object.type) return 0;
  else if (type == String)
    return xstreq(arg1->String.val, arg2->String.val);
  else if (type == Symbol)
    return xstreq(arg1->Symbol.name, arg2->Symbol.name);
  else if (type == Char) return arg1->Char.val == arg2->Char.val;
  else if (type == Number) return arg1->Number.val == arg2->Number.val;
  else return arg1 == arg2;
}

static S *S_alloc() {
  S *expr;
  if (NILP(pool)) expr = xmalloc(sizeof(S));
  else {
    expr = pool;
    pool = REST(expr);
  }
  expr->Object.age = GC_NEWBIE;
  return expr;
}

S *Cons_new(S *car, S *cdr) {
  S *root;
  assert(!ATOMP(cdr) || NILP(cdr));
  root = S_alloc();
  root->Cons.type = Cons;
  root->Keyword.age = GC_PERM;
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
  if (strlen(val) == 0) return Error_msg("read: Illegal Keyword `'.");
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
  if (val == '\0') return Error_msg("read: Illegal character.");
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
  if (NILP(type)) {
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
    if ((gTo = Function_lookup(fnTo, gFrom->type)) != NULL && gTo->type == nil)
      continue;
    if (gTo == NULL)
      Function_addGeneric(
          fnTo, gFrom->type, gFrom->params, gFrom->body, gFrom->prim);
    else {
      gTo->params = gFrom->params;
      gTo->body = gFrom->body;
      gTo->prim = gFrom->prim;
    }
  }
  if ((gFrom = fnFrom->Function.gDefault) != NULL) {
    if ((gTo = fnTo->Function.gDefault) == NULL)
      Function_addGeneric(fnTo, nil, gFrom->params, gFrom->body, gFrom->prim);
    else {
      gTo->params = gFrom->params;
      gTo->body = gFrom->body;
      gTo->prim = gFrom->prim;
    }
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

S *Error_new(S *args, ...) {
  S *err, *acc, *arg;
  va_list va;
  err = S_alloc();
  err->Error.type = Error;
  va_start(va, args);
  acc = nil;
  for (arg = args; !NILP(arg); arg = va_arg(va, S *)) acc = Cons_new(arg, acc);
  va_end(va);
  err->Error.args = REVERSE(acc);
  return err;
}

S *Error_msg(char *msg) {
  return Error_new(String_new(msg), nil);
}

static S *Error_illegalArgument(S *fn, int provided, int min, int max) {
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
      String_new(xvstrcat("eval: Too ", s1, " arguments in call to \n`"))
      , fn , String_new(xvstrcat(
          "' " , xitoa(provided), " arguments provided, at ", s2, s3, s4))
      , nil);
  free(s3);
  return err;
}

void Cons_free(S *expr) {
  S_free(expr->Cons.cdr);
  REST(expr) = REST(pool);
  REST(pool) = expr;
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

void S_free(S *expr) {
  if (TYPEP(expr, Cons)) Cons_free(expr);
  else if (TYPEP(expr, Symbol)) Symbol_free(expr);
  else if (TYPEP(expr, Keyword) || TYPEP(expr, Special)) return;
  else if (TYPEP(expr, String)) String_free(expr);
  else if (TYPEP(expr, Macro)) Macro_free(expr);
  else if (TYPEP(expr, Function)) Function_free(expr);
  else free(expr);
}

// special forms

static S *Special_if(S *expr) {
  int len;
  if ((len = LENGTH(expr)) < 2)
    return Error_illegalArgument(String_new("if"), len, 2, -1);
  while (!NILP(REST(expr))) {
    if (!NILP(EVAL(FIRST(expr)))) return EVAL(SECOND(expr));
    if (NILP(expr = REST(expr)) || NILP(expr = REST(expr))) return nil;
  }
  return EVAL(FIRST(expr));
}

static S *Special_quote(S *expr) {
  int len;
  if ((len = LENGTH(expr)) > 1)
    return Error_illegalArgument(quote, len, 1, 1);
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
    return Error_msg("let: Illegal parameter.");
  for (cons = params; !NILP(cons); cons = REST(REST(cons)))
    if (!TYPEP(FIRST(cons), Symbol))
      return Error_msg("let: Parameter must be symbol.");
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
  if ((LENGTH(expr) % 2) != 0) return Error_msg("<-: Illegal argument.");
  for (cons = expr; !NILP(cons); cons = REST(REST(cons))) {
    if (!TYPEP(var = FIRST(cons), Symbol))
      return Error_msg("<-: variable must be symbol.");
    if (Env_getSymbol(&env, var->Symbol.name) == NULL)
      return Error_msg(
          xvstrcat("<-: Undefined variable `", var->Symbol.name,  "'."));
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
      return Error_msg("def: variable must be symbol.");
    if (Env_getSymbol(&env, var->Symbol.name) != NULL)
      return Error_msg("def: variable already defined.");
  }
  for (cons = expr; !NILP(cons); cons = REST(cons))
    Env_putSymbol(&env, FIRST(cons)->Symbol.name, nil);
  return nil;
}

static S *Special_macro(S *expr) {
  if (LENGTH(expr) < 1 || ATOMP(FIRST(expr)))
    return Error_msg("macro: Illegal argument.");
  return Macro_new(FIRST(expr), REST(expr));
}

static S *Error_illegalParameter(S *params) {
  return Error_new(
      String_new("fn: But parameter list ")
      , params, String_new("."), nil);
}

static S *Special_fn(S *expr) {
  int len;
  S *type, *params, *body;
  if ((len = LENGTH(expr)) < 2)
    return Error_illegalArgument(String_new("fn"), len, 2, -1);
  if (TYPEP(FIRST(expr), Symbol)) {
    type = nil;
    params = Cons_new(dot, Cons_new(FIRST(expr), nil));
    body = REST(expr);
  } else if (TYPEP(FIRST(expr), Keyword)) {
    type = FIRST(expr);
    params = SECOND(expr);
    body = REST(REST(expr));
  } else {
    type = nil;
    params = FIRST(expr);
    body = REST(expr);
  }
  if (!LISTP(params)) return Error_illegalParameter(params);
  for (expr = params; !NILP(expr); expr = REST(expr)) {
    if (!TYPEP(FIRST(expr), Symbol)) return Error_illegalParameter(params);
    if (EQ(FIRST(expr), dot)) {
      if (NILP(expr = REST(expr)))
        return Error_msg("fn: Not declared after variable argument.");
      if (!NILP(REST(expr))) return Error_illegalParameter(params);
      break;
    }
  }
  return Function_new(type, params, body, NULL);
}

// primitive functions.

static S *Function_isEqual(S *args) {
  int len;
  if ((len = LENGTH(args)) < 2)
    return Error_illegalArgument(String_new("=?"), len, 2, -1);
  while (!NILP(REST(args))) {
    if (!EQ(FIRST(args), SECOND(args))) return nil;
    args = REST(args);
  }
  return t;
}

static S *Function_isNil(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument(String_new("nil?"), len, 1, 1);
  return NILP(FIRST(args))? t: nil;
}

static S *Function_isAtom(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument(String_new("atom?"), len, 1, 1);
  return ATOMP(FIRST(args))? t: nil;
}

static S *Function_car(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument(String_new("car"), len, 1, 1);
  if (NILP(FIRST(args))) return nil;
  if (!TYPEP(FIRST(args), Cons)) return Error_msg("car: Not a list.");
  return FIRST(FIRST(args));
}

static S *Function_cdr(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument(String_new("cdr"), len, 1, 1);
  if (NILP(FIRST(args))) return nil;
  if (!TYPEP(FIRST(args), Cons)) return Error_msg("cdr: Not a list.");
  return REST(FIRST(args));
}

static S *Function_cons(S *args) {
  int len;
  if ((len = LENGTH(args)) != 2)
    return Error_illegalArgument(String_new("cons"), len, 2, 2);
  if (!TYPEP(SECOND(args), Cons) && !NILP(SECOND(args)))
    return Error_msg("cons: Must be terminated with nil.");
  return Cons_new(FIRST(args), SECOND(args));
}

static S *Function_open(S *args) {
  int len;
  FILE *fp;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument(String_new("open"), len, 1, 1);
  if ((fp = fopen(FIRST(args)->String.val, "r")) == NULL)
    return Error_msg("String.open: cannnot open.");
  return Stream_new(fp);
}

static S *Function_close(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument(String_new("close"), len, 1, 1);
  fclose(FIRST(args)->Stream.fp);
  FIRST(args)->Symbol.type = Symbol;
  FIRST(args)->Symbol.name = "nil";
  return nil;
}

static S *Function_getChar(S *args) {
  int len, c;
  if ((len = LENGTH(args)) > 2)
    return Error_illegalArgument(String_new("getChar"), len, 1, 2);
  c = fgetc(FIRST(args)->Stream.fp);
  if (c != EOF) return Char_new(c);
  else if (len == 2) return SECOND(args);
  else return eof;
}

static S *Function_putChar(S *args) {
  int len;
  S *c;
  if ((len = LENGTH(args)) != 2)
    return Error_illegalArgument(String_new("putChar"), len, 2, 2);
  if (!TYPEP(c = SECOND(args), Char))
    return Error_msg("putChar: Second argument must be character.");
  fputc(c->Char.val, FIRST(args)->Stream.fp);
  return c;
}

static S *Function_Number_add(S *args) {
  S *acc;
  acc = Number_new(0);
  while (!NILP(args)) {
    if (!TYPEP(FIRST(args), Number)) return Error_msg("+: is not a number");
    acc->Number.val += FIRST(args)->Number.val;
    args = REST(args);
  }
  return acc;
}

static S *Function_Number_mul(S *args) {
  S *acc;
  acc = Number_new(1);
  while (!NILP(args)) {
    if (!TYPEP(FIRST(args), Number)) return Error_msg("*: is not a number");
    acc->Number.val *= FIRST(args)->Number.val;
    args = REST(args);
  }
  return acc;
}

static S *Function_read(S *args) {
  int len;
  Reader rd;
  if ((len = LENGTH(args)) > 2)
    return Error_illegalArgument(String_new("read"), len, 1, 2);
  Reader_init(&rd, FIRST(args)->Stream.fp, ((len == 2)? SECOND(args): eof));
  return Reader_read(&rd);
}

static S *Function_eval(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument(String_new("eval"), len, 1, 1);
  return EVAL(FIRST(args));
}

static S *Function_apply(S *args) {
  int len;
  S *fn, *acc;
  if ((len = LENGTH(args)) < 2)
    return Error_illegalArgument(String_new("apply"), len, 2, -1);
  if (!TYPEP(fn = FIRST(args), Function))
    return Error_msg("apply: First argument type must be Function.");
  if (ATOMP(FIRST(acc = REVERSE(REST(args)))))
    return Error_msg("apply: Last argument must be List.");
  for (args = FIRST(acc), acc = REST(acc); !NILP(acc); acc =REST(acc))
    args = Cons_new(FIRST(acc), args);
  return APPLY(fn, args);
}

static S *Function_Stream_put(S *args) {
  int len;
  Writer wr;
  if ((len = LENGTH(args)) != 2)
    return Error_illegalArgument(String_new("put"), len, 2, 2);
  Writer_init(&wr, FIRST(args)->Stream.fp);
  Writer_write(&wr, SECOND(args));
  return SECOND(args);
}

static S *Function_typeOf(S *args) {
  int len;
  if ((len = LENGTH(args)) != 1)
    return Error_illegalArgument(String_new("typeOf"), len, 1, 1);
  return FIRST(args)->Object.type;
}

S *APPLY(S *fn, S *args) {
  int len, min, max, isVariable;
  S *params, *body, *result;
  if (TYPEP(fn, Macro)) {
    params = fn->Macro.params;
    body = fn->Macro.body;;
  }
  else {
    struct Generic *g;
    if ((g = Function_lookup(fn, FIRST(args)->Object.type)) == NULL)
      return Error_new(
          String_new("eval: Method not found generic function in \n"), fn
          , String_new(xvstrcat(
              "\ntype of `", FIRST(args)->Object.type->Keyword.val, "'."))
          , nil);
    // invoke primitive function.
    if (g->params == NULL) return g->prim(args);
    params = g->params;
    body = g->body;
  }
  Env_push(&env);
  min = isVariable = 0;
  for (result = params; !NILP(result); result = REST(result), min++)
    if (EQ(FIRST(result), dot)) {
      isVariable = 1;
      break;
    }
  max = isVariable? -1: min;
  if ((len = LENGTH(args)) < min || (max != -1 && len > max))
    return Error_illegalArgument(fn, len, min, max);
  while (!NILP(params)) {
    if (EQ(FIRST(params), dot)) {
      Env_putSymbol(&env, SECOND(params)->Symbol.name, args);
      break;
    }
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
    return Error_msg(
        xvstrcat("eval: undefined variable `", expr->Symbol.name, "'."));
  }
  if (LENGTH(expr) <= 1)
    return Error_new(String_new("eval: Expression `")
        , expr, String_new("' not found receiver."), nil);
  if (TYPEP(fn = EVAL(FIRST(expr)), Error)) return fn;
  // special form
  if (TYPEP(fn, Special)) return (fn->Special.fn)(REST(expr));
  // macro
  if (TYPEP(fn, Macro)) return EVAL(APPLY(fn, REST(expr)));
  // function
  if (!TYPEP(fn, Function))
    return Error_new(
        String_new("eval: The value of `"), fn
        , String_new("' is not a function."), nil);
  acc = nil;
  while (!NILP(expr = REST(expr))) {
    acc = Cons_new(EVAL(FIRST(expr)), acc);
    if (TYPEP(FIRST(acc), Error)) return FIRST(acc);
  }
  return APPLY(fn, REVERSE(acc));
}

void Paren_init() {

  Env_init(&env);
  pool = nil;

  // init primitive types.
  Keyword = Keyword_new("Keyword");
  Keyword->Keyword.type = Keyword;
  t = Keyword_new("t");
  pool = nil = Keyword_new("nil");
  eof = Keyword_new("EOF");
  Cons = Keyword_new("Cons");
  Symbol = Keyword_new("Symbol");
  String = Keyword_new("String");
  Char = Keyword_new("Char");
  Number = Keyword_new("Number");
  Special = Keyword_new("Special");
  Macro = Keyword_new("Macro");
  Function = Keyword_new("Function");
  Stream = Keyword_new("Stream");
  Error = Keyword_new("Error");

  // init global symbols.
  quote = Symbol_new("quote");
  dot = Symbol_new(".");
  Env_putSymbol(&env, "stdin", Stream_new(stdin));
  Env_putSymbol(&env, "stdout", Stream_new(stdout));
  Env_putSymbol(&env, "stderr", Stream_new(stderr));
  Env_putSymbol(&env, "pi", Number_new(3.14159265358979323846));

  // init special forms.
  Env_putSymbol(&env, "<-", Special_new(Special_assign));
  Env_putSymbol(&env, "def", Special_new(Special_def));
  Env_putSymbol(&env, "macro", Special_new(Special_macro));
  Env_putSymbol(&env, "fn", Special_new(Special_fn));
  Env_putSymbol(&env, "if", Special_new(Special_if));
  Env_putSymbol(&env, "let", Special_new(Special_let));
  Env_putSymbol(&env, "progn", Special_new(Special_progn));
  Env_putSymbol(&env, "quote", Special_new(Special_quote));

  // init functions.
  Env_putSymbol(&env, "atom?", Function_new(nil, NULL, NULL, Function_isAtom));
  Env_putSymbol(&env, "car", Function_new(nil, NULL, NULL, Function_car));
  Env_putSymbol(&env, "cdr", Function_new(nil, NULL, NULL, Function_cdr));
  Env_putSymbol(&env, "close", Function_new(Stream, NULL, NULL, Function_close));
  Env_putSymbol(&env, "cons", Function_new(nil, NULL, NULL, Function_cons));
  Env_putSymbol(&env, "eval", Function_new(nil, NULL, NULL, Function_eval));
  Env_putSymbol(&env, "apply", Function_new(nil, NULL, NULL, Function_apply));
  Env_putSymbol(&env, "getChar", Function_new(Stream, NULL, NULL, Function_getChar));
  Env_putSymbol(&env, "nil?", Function_new(nil, NULL, NULL, Function_isNil));
  Env_putSymbol(&env, "open", Function_new(String, NULL, NULL, Function_open));
  Env_putSymbol(&env, "put", Function_new(Stream, NULL, NULL, Function_Stream_put));
  Env_putSymbol(&env, "putChar", Function_new(Stream, NULL, NULL, Function_putChar));
  Env_putSymbol(&env, "read", Function_new(Stream, NULL, NULL, Function_read));
  Env_putSymbol(&env, "+", Function_new(Number, NULL, NULL, Function_Number_add));
  Env_putSymbol(&env, "*", Function_new(Number, NULL, NULL, Function_Number_mul));
  Env_putSymbol(&env, "=", Function_new(nil, NULL, NULL, Function_isEqual));
  Env_putSymbol(&env, "typeOf", Function_new(nil, NULL, NULL, Function_typeOf));

  // init reader and writer.
  Reader_init(&rd, stdin, eof);
  Writer_init(&wr, stdout);

}

static void Paren_errorHandler(S *expr) {
  Writer_write(&wr, expr);
}

static void Paren_prompt() {
  printf(") ");
}

int main(int argc, char* argv[]) {
  void *expr;
  setbuf(stdout, NULL);
  Paren_init();
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
