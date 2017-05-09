/*
  s-expression.
*/

#ifndef IS_LOADED_SEXPR
#define IS_LOADED_SEXPR

// TODO: define second, third with first, rest.
#define FIRST(expr) (expr->Cons.car)
#define SECOND(expr) ((expr->Cons.cdr)->Cons.car)
#define THIRD(expr) (((expr->Cons.cdr)->Cons.cdr)->Cons.car)
#define REST(expr) (expr->Cons.cdr)

typedef enum {
  Cons,
  Map,
  Symbol,
  Keyword,
  String,
  Character,
  Number,
  Function,
  Stream,
  Error
} Type;

extern char *TypeString[10];

typedef union S {
  struct {
    Type type;
    union S *car, *cdr;
  } Cons;
  struct {
    Type type;
    struct MapNode {
      union S *key;
      union S *val;
      struct MapNode *next;
    } *head;
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
    union S *args;
    union S *(* f)(union S *);
  } Function;
  struct {
    Type type;
    FILE *stream;
  } Stream;
  struct {
    Type type;
    char *val;
  } Error;
} S;

extern S *Cons_new(S *car, S *cdr);
extern S *Map_new(S *car, S *cdr);
extern S *Symbol_new(char *val);
extern S *Keyword_new(char *val);
extern S *String_new(char *val);
extern S *Character_new(char val);
extern S *Number_new(double val);
extern S *Function_new(S *f(S *), S *args);
extern S *Stream_new(FILE *stream);
extern S *Error_new(char *val);

#endif
