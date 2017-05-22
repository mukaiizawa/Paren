/*
   paren primitive.
   */

typedef enum {
  Cons,
  Type,
  Symbol,
  Keyword,
  String,
  Character,
  Number,
  Function,
  Special,
  Stream,
  Error
} ParenType;

extern char *Type_asString(ParenType type);

typedef union S {
  struct {
    ParenType type;
    union S *car, *cdr;
  } Cons;
  struct {
    ParenType type;
    char *val;
    union S *super;
  } Type;
  struct {
    ParenType type;
    char *name;
    Splay vars;
  } Structure;
  struct {
    ParenType type;
    char *val;
  } Symbol;
  struct {
    ParenType type;
    char *val;
  } Keyword;
  struct {
    ParenType type;
    char *val;
  } String;
  struct {
    ParenType type;
    char val;
  } Character;
  struct {
    ParenType type;
    double val;
  } Number;
  struct {
    ParenType type;
    union S *(* f)(union S *, Env *env);
  } Special;
  struct {
    ParenType type;
    union S *signature;
    union S *args;
    union S *body;
    union S *(* prim)(union S *);
  } Function;
  struct {
    ParenType type;
    FILE *stream;
  } Stream;
  struct {
    ParenType type;
    char *val;
  } Error;
} S;

#define FIRST(expr) (expr->Cons.car)
#define REST(expr) (expr->Cons.cdr)
#define SECOND(expr) FIRST(REST(expr))
#define THIRD(expr) FIRST(REST(REST(expr)))
#define ATOMP(expr) (expr->Cons.type != Cons)
#define NILP(expr) ((expr) == nil)
extern int LENGTH(S *expr);
extern int S_isType(S *expr, ParenType t);

extern S *t;
extern S *nil;

extern S *Cons_new(S *car, S *cdr);
extern S *Type_new(char *val, S* super);
extern S *Symbol_new(char *val);
extern S *Keyword_new(char *val);
extern S *String_new(char *val);
extern S *Character_new(char val);
extern S *Number_new(double val);
extern S *Function_new(S *signature, S *args, S *body, S *prim(S *));
extern S *Special_new(S *f(S *, Env *));
extern S *Stream_new(FILE *stream);
extern S *Error_new(char *val);

extern S *S_read();
extern S *S_eval(S *expr, Env *env);
extern S *S_print(S *expr);

extern void Prim_init(Env *env);
