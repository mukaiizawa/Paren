/*
  paren primitive.
*/

#define FIRST(expr) (expr->Cons.car)
#define REST(expr) (expr->Cons.cdr)
#define SECOND(expr) FIRST(REST(expr))
#define THIRD(expr) FIRST(REST(REST(expr)))
#define ATOMP(expr) (expr->Cons.type != Cons)
#define NILP(expr) ((expr) == nil)

typedef enum {
  Cons,
  Map,
  Symbol,
  Keyword,
  String,
  Character,
  Number,
  Function,
  Special,
  Stream,
  Error
} Type;

extern char *Type_asString(Type type);

typedef union S {
  struct {
    Type type;
    union S *car, *cdr;
  } Cons;
  struct {
    Type type;
    Splay map;
  } Map;
  struct {
    Type type;
    char *val;
  } Symbol;
  struct {
    Type type;
    char *val;
  } Keyword;
  struct {
    Type type;
    char *val;
  } String;
  struct {
    Type type;
    char val;
  } Character;
  struct {
    Type type;
    double val;
  } Number;
  struct {
    Type type;
    union S *(* f)(union S *);
    union S *args;
  } Function;
  struct {
    Type type;
    union S *(* f)(union S *);
  } Special;
  struct {
    Type type;
    FILE *stream;
  } Stream;
  struct {
    Type type;
    char *val;
  } Error;
} S;

extern int LENGTH(S *expr);
extern int S_isType(S *expr, Type t);

extern S *t;
extern S *nil;

extern S *Cons_new(S *car, S *cdr);
extern S *Map_new();
extern S *Symbol_new(char *val);
extern S *Keyword_new(char *val);
extern S *String_new(char *val);
extern S *Character_new(char val);
extern S *Number_new(double val);
extern S *Function_new(S *f(S *), S *args);
extern S *Special_new(S *f(S *));
extern S *Stream_new(FILE *stream);
extern S *Error_new(char *val);

extern void Prim_initSymbolTable(Env *env);
