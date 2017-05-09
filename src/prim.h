/*
  paren primitive.
*/

extern void Prim_init(S *env);

// global variable
extern S *t;
extern S *nil;
extern S *in;
extern S *out;
extern S *err;

// method
extern S *S_read();
extern S *S_eval(S *expr, S *env);
extern S *S_print(S *expr);
extern S *S_reverse(S *expr);
extern int S_length(S *expr);
extern int S_isAtom(S *expr);
extern int S_isNil(S *expr);
