// garbage collector

extern int gc_used_memory;
extern int gc_max_used_memory;

extern object gc_new_env(object top);
extern object gc_new_macro(object env, object params, object body);
extern object gc_new_lambda(object env, object params, object body);
extern object gc_new_builtin(int type, object name, void *p);
extern object gc_new_bytes(int64_t val);
extern object gc_new_xint(int64_t val);
extern object gc_new_xfloat(double val);
extern object gc_new_cons(object car, object cdr);
extern object gc_new_barray(int type, int size);
extern object gc_new_barray_from(int type, char *val, int size);
extern object gc_new_array(int size);
extern object gc_new_array_from(object *o, int size);
extern object gc_new_pointer(void *p);

extern object gc_new_splay(object cmp);
extern object gc_new_splay_node(object k, object v, object l, object r);

extern object gc_new_throwable(object e, char *msg);

extern void gc_init(void);
extern void gc_mark(object o);
extern void gc_chance(void);
