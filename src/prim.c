/*
  paren primitive.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "splay.h"
#include "env.h"
#include "prim.h"

S *t;
S *nil;

static char *PARENTYPE_STRING[11] = {
  "Cons",
  "Type",
  "Symbol",
  "Keyword",
  "String",
  "Character",
  "Number",
  "Function",
  "Special",
  "Stream",
  "Error"
};

int S_isType(S *expr, ParenType t) {
  return expr->Cons.type == t;
};

int LENGTH(S *expr) {
  int count;
  if(expr == nil) return 0;
  if (ATOMP(expr)) return 1;
  count = 1;
  while (!NILP((expr = REST(expr)))) count++;
  return count;
}

char *ParenType_asString(ParenType type) {
  return PARENTYPE_STRING[type];
};

static S *S_alloc() {
  S *expr;
  if ((expr = (S *)malloc(sizeof(S))) == NULL) {
    fprintf(stderr, "S_alloc: Cannot allocate memory.");
    exit(1);
  }
  return expr;
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

S *Type_new(char *val, S* super) {
  S *expr;
  expr = S_alloc();
  expr->Type.type = Type;
  expr->Type.val = val;
  expr->Type.super = super;
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

S *Character_new(char val) {
  S *expr;
  if (val == '\0')
    return Error_new("Illegal character.");
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

// TODO: 総称関数のマッチ判定処理
static struct Generic *Function_lookupGenerics(S *fn, S *signature) {
  struct Generic *generic;
  return generic;
}

// TODO: 同一シグネチャが存在する場合は入れ替え
static void Function_addGenerics(
    S *fn, S *signature, S *args, S *body, S *prim(S *))
{
  struct Generic *g;
  assert(S_isType(fn, Function));
  g = Generics_alloc();
  g->signature = signature;
  g->args = args;
  g->body = body;
  g->prim = prim;
  g->next = fn->Function.generics;
  fn->Function.generics = g;
}

S *Function_new(S *signature, S *args, S *body, S *prim(S *)) {
  S *expr;
  expr = S_alloc();
  expr->Function.type = Function;
  expr->Function.generics = NULL;
  Function_addGenerics(expr, signature, args, body, prim);
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
  else if (S_isType(expr, Cons))
    return Number_new(LENGTH(FIRST(expr)));
  else
    return Error_new("length: Cannot apply.");
}

// TODO:
static S *Function_desc(S *expr) {
  printf("address: %d\n", (int)expr);
  printf("type: %s\n", ParenType_asString(expr->Cons.type));
  switch (expr->Cons.type) {
    case Cons:
      printf("car: %d\n", (int)FIRST(expr));
      printf("cdr: %d\n", (int)REST(expr));
      break;
    case Symbol:
      printf("name: %s\n", expr->Symbol.val);
      break;
    case Keyword:
      printf("name: %s\n", expr->Keyword.val);
      break;
    case String:
      printf("value: %s\n", expr->String.val);
      break;
    case Character:
      printf("value: %c\n", expr->Character.val);
      break;
    case Number:
      printf("value: %f\n", expr->Number.val);
      break;
    case Function:
      break;
    case Stream:
      break;
    case Error:
      break;
    default:
      break;
  }
  return expr;
}

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
  S *varVal, *result;
  if (!(LENGTH(expr) % 2 == 0))
    return Error_new("let: Arguments must be even number.");
  if (!S_isType(FIRST(expr), Cons))
    return Error_new("let: First expression must be list.");
  Env_push(env);
  for (varVal = FIRST(expr); !NILP(varVal); varVal = REST(REST(varVal))) {
    if (!S_isType(FIRST(varVal), Symbol))
      return Error_new("let: variable is not a symbol.");
    Env_putSymbol(env, FIRST(varVal)->Symbol.val, S_eval(SECOND(varVal), env));
  }
  result = Special_progn(REST(expr), env);
  Env_pop(env);
  return result;
}

static S *Special_assign(S *expr, Env *env) {
  S *var, *val;
  if (!(LENGTH(expr) % 2 == 0))
    return Error_new("<-: Arguments must be even number.");
  for (; !NILP(expr); expr = REST(REST(expr))) {
    var = FIRST(expr);
    if (!S_isType(var, Symbol))
      return Error_new("<-: variable must be symbol.");
    if ((val = (S *)Env_getSymbol(env, var->Symbol.val, NULL)) == NULL)
      return Error_new("<-: undefined variable.");
    Env_putSymbol(env, var->Symbol.val, val = S_eval(SECOND(expr), env));
  }
  return val;
}

static S *Special_defVar(S *expr, Env *env) {
  S *var, *val;
  if (!(LENGTH(expr) % 2 == 0))
    return Error_new("defvar: Arguments must be even number.");
  for (; !NILP(expr); expr = REST(REST(expr))) {
    var = FIRST(expr);
    if (!S_isType(var, Symbol))
      return Error_new("defvar: variable must be symbol.");
    Env_putSymbol(env, var->Symbol.val, val = S_eval(SECOND(expr), env));
  }
  return val;
}

// // (fn (Object o String... s) body)
// static S *Special_fn(S *expr, Env *env) {
//   return NULL;
// }

static S *S_errorHandler(S *expr) {
  S_print(expr);
  exit(1);
}

extern S *Lex_parseExpr();

S *S_read() {
  return Lex_parseExpr();
}

S *S_eval(S *expr, Env *env) {
  S *root, *cmd, *args;
  if (ATOMP(expr)) {
    if (S_isType(expr, Symbol))
      expr = (S *)Env_getSymbol(env, expr->Symbol.val
          , Error_new("eval: undefined variable."));
    return S_isType(expr, Error)?
      S_errorHandler(expr):
      expr;
  }
  if ((S *)(cmd = Env_getSpecial(env, FIRST(expr)->Symbol.val)) != NULL)
    return (cmd->Special.fn)(REST(expr), env);
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
  // // invoke primitive function.
  // if (cmd->Function.fn != NULL)
  //   return (cmd->Function.fn)(args);
  // // invoke user defined function.
  // else {
  //   return nil;    // TODO: apply user defined function.
  // }
  return nil;
}

S *S_print(S *expr) {
  if (ATOMP(expr)) {
    if (S_isType(expr, Number)) {
      double intptr, fraction;
      fraction = modf(expr->Number.val, &intptr);
      if (fraction == 0) printf("%d", (int)intptr);
      else printf("%f", expr->Number.val);
    }
    else if (S_isType(expr, Character)) printf("%c", expr->Character.val);
    else if (S_isType(expr, Function)) printf("%d", expr->Function.type);
    else if (S_isType(expr, Keyword)) printf(":%s", expr->Keyword.val);
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

// primitive types
S *Type_Object;

void Prim_init(Env *env) {
  // init special forms.
  Env_putSpecial(env, "ifElse", Special_new(Special_ifElse));
  Env_putSpecial(env, "quote", Special_new(Special_quote));
  Env_putSpecial(env, "progn", Special_new(Special_progn));
  Env_putSpecial(env, "let", Special_new(Special_let));
  Env_putSpecial(env, "<-", Special_new(Special_assign));
  Env_putSpecial(env, "defVar", Special_new(Special_defVar));
  // Env_putSpecial(env, "defStruct", Special_new(Special_defStruct));

  // init global symbols.
  Env_putSymbol(env, "t", (t = Symbol_new("t")));
  Env_putSymbol(env, "nil", (nil = Symbol_new("nil")));
  Env_putSymbol(env, "stdin", Stream_new(stdin));
  Env_putSymbol(env, "stdout", Stream_new(stdout));
  Env_putSymbol(env, "stderr", Stream_new(stderr));
  Env_putSymbol(env, "pi", Number_new(3.14159265358979323846));

  // init types.
  Env_putType(env, "Object", Type_Object = Type_new("Object", nil));

  // init functions.
  Env_putSymbol(env, "nil?"
      , Function_new(Cons_new(Type_Object, nil), NULL, NULL, Function_isNil));
  // Env_putSymbol(env, "atom?", Function_new(Function_isAtom, NULL));
  // Env_putSymbol(env, "car", Function_new(Function_car, NULL));
  // Env_putSymbol(env, "cdr", Function_new(Function_cdr, NULL));
  // Env_putSymbol(env, "cons", Function_new(Function_cons, NULL));
  // Env_putSymbol(env, "list", Function_new(Function_list, NULL));
  // Env_putSymbol(env, "length", Function_new(Function_length, NULL));
  // Env_putSymbol(env, "desc", Function_new(Function_desc, NULL));
}
