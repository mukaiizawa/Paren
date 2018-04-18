// garbage collector

extern object toplevel;

extern object gc_new_lambda(object top, object params, object body
    , int prim_cd);
extern object gc_new_xint(int val);
extern object gc_new_xfloat(double val);
extern object gc_new_cons(object car, object cdr);
extern object gc_new_symbol(char *name);
extern object gc_new_barray(int len);
extern object gc_new_fbarray(int len);

extern void gc_full(void);
extern void gc_init(void);
extern void gc_dump_table(void);
