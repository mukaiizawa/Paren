// garbage collector

extern int gc_used_memory;
extern int gc_max_used_memory;

extern object gc_new_env(object top);
extern object gc_new_macro(object env, object params, object body);
extern object gc_new_lambda(object env, object params, object body);
extern object gc_new_bytes(int val);
extern object gc_new_xint(int val);
extern object gc_new_xfloat(double val);
extern object gc_new_cons(object car, object cdr);
extern object gc_new_symbol(char *name);
extern object gc_new_barray(int size);

extern void gc_init(void);
extern void gc_mark(object o);
extern void gc_chance(void);
extern void gc_dump_table(void);
