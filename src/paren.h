/*
  paren primitive.
*/

#define HEADER \
  int age; \
  int isAlive; \
  union S *type;

typedef union S {
  struct {
    HEADER;
  } Object;
  struct {
    HEADER;
    union S *car, *cdr;
  } Cons;
  struct {
    HEADER;
    char *name;
  } Symbol;
  struct {
    HEADER;
    char *val;
  } Keyword;
  struct {
    HEADER;
    char *val;
  } String;
  struct {
    HEADER;
    char val;
  } Char;
  struct {
    HEADER;
    double val;
  } Number;
  struct {
    HEADER;
    union S *(* fn)(union S *);
  } Special;
  struct {
    HEADER;
    union S *params;
    union S *body;
  } Macro;
  struct {
    HEADER;
    struct Generic {
      union S *type;
      union S *params;
      union S *body;
      union S *(* prim)(union S *);
      struct Generic *next;
    } *generics;
    struct Generic *gDefault;
  } Function;
  struct {
    HEADER;
    FILE *fp;
  } Stream;
  struct {
    HEADER;
    union S *args;
  } Error;
} S;

#define FIRST(expr) ((expr)->Cons.car)
#define REST(expr) ((expr)->Cons.cdr)
#define SECOND(expr) FIRST(REST(expr))
#define THIRD(expr) FIRST(REST(REST(expr)))
#define ATOMP(expr) ((expr)->Cons.type != Cons)
#define NILP(expr) ((expr) == nil)
#define LISTP(expr) (NILP(expr) || !ATOMP(expr))
extern int LENGTH(S *expr);
extern int TYPEP(S *expr, S *type);
extern int EQ(S *arg1, S *arg2);
extern S *REVERSE(S *expr);

extern Env env;

extern S *t;
extern S *nil;
extern S *eof;
extern S *dot;
extern S *quote;

extern S *Cons;
extern S *Symbol;
extern S *Keyword;
extern S *String;
extern S *Char;
extern S *Number;
extern S *Macro;
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
extern S *Function_new(S *type, S *params, S *body, S *prim(S *));
extern S *Special_new(S *f(S *));
extern S *Macro_new(S *params, S *body);
extern S *Stream_new(FILE *stream);
extern S *Error_new(S *args, ...);
extern S *Error_msg(char *msg);
extern void S_free(S *expr);
