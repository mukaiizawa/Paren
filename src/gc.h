// garbage collector.

extern object gc_new_xint(int64_t val);
extern object gc_new_xfloat(double val);
extern object gc_new_cons(object car, object cdr);
extern object gc_copy_cons(object o, object *tail);
extern object gc_new_mem(int type, int size);
extern object gc_new_mem_from(int type, char *val, int size);
extern object gc_new_mem_from_cstr(int type, char *cstr);
extern object gc_new_mem_from_xbarray(int type, struct xbarray *x);
extern object gc_new_cstring(object o);
extern object gc_new_array(int size);
extern object gc_new_array_from(object *o, int size);
extern object gc_new_dict(void);
extern object gc_new_func(object env, int param_count, object params, object body);
extern object gc_new_macro(object env, int param_count, object params, object body);
extern object gc_new_native(int type, object name, void *p);
extern object gc_new_env(object top, int half_size);
extern void gc_extend_table(object o);

extern void gc_init(void);
extern void gc_mark(object o);
extern void gc_chance(void);
