/*
  paren primitive.
*/

typedef union S {
  struct {
    union S *type;
  } Type;
  struct {
    union S *type;
    union S *car, *cdr;
  } Cons;
  struct {
    union S *type;
    char *name;
    Splay vars;
  } Structure;
  struct {
    union S *type;
    char *name;
  } Symbol;
  struct {
    union S *type;
    char *val;
  } Keyword;
  struct {
    union S *type;
    char *val;
  } String;
  struct {
    union S *type;
    char val;
  } Char;
  struct {
    union S *type;
    double val;
  } Number;
  struct {
    union S *type;
    union S *(* fn)(union S *, Env *env);
  } Special;
  struct {
    union S *type;
    struct Generic {
      union S *type;
      union S *args;
      union S *body;
      union S *(* prim)(union S *);
      struct Generic *next;
    } *generics;
  } Function;
  struct {
    union S *type;
    FILE *stream;
  } Stream;
  struct {
    union S *type;
    char *val;
  } Error;
} S;

#define FIRST(expr) ((expr)->Cons.car)
#define REST(expr) ((expr)->Cons.cdr)
#define SECOND(expr) FIRST(REST(expr))
#define THIRD(expr) FIRST(REST(REST(expr)))
#define ATOMP(expr) ((expr)->Cons.type != Cons)
#define NILP(expr) ((expr) == nil)
extern int LENGTH(S *expr);
extern int TYPEP(S *expr, S *type);

extern S *t;
extern S *nil;

extern S *Cons;
extern S *Symbol;
extern S *Keyword;
extern S *Char;
extern S *Number;
extern S *Function;
extern S *Special;
extern S *Stream;
extern S *Error;

extern S *Cons_new(S *car, S *cdr);
extern S *Symbol_new(char *name);
extern S *Keyword_new(char *val);
extern S *String_new(char *val);
extern S *Char_new(char val);
extern S *Number_new(double val);
extern S *Function_new(S *type, S *args, S *body, S *prim(S *));
extern S *Special_new(S *f(S *, Env *));
extern S *Stream_new(FILE *stream);
extern S *Error_new(char *val);

extern S *S_read();
extern S *S_eval(S *expr, Env *env);
extern S *S_print(S *expr);

extern void Prim_init(Env *env);
