/*
  paren primitive.
*/

#ifndef IS_LOADED_PRIM
#define IS_LOADED_PRIM

enum Type {
  Symbol,
  Keyword,
  String,
  Character,
  Number,
  Function,
  Error,
  Cons,
  Nil
};

union S {
  struct {
    Type type;
    union S *prev, *car, *cdr;
  } Cons;
  struct {
    Type type;
    struct S *prev;
    char *string;
    char character;
    double number;
  } Atom;
};

extern struct S *S_newSymbol(char *val);
extern struct S *S_newKeyword(char *str);
extern struct S *S_newString(char *str);
extern struct S *S_newCharacter(char *str);
extern struct S *S_newNumber(char *str);
extern struct S *asString(struct S *obj);

#endif
