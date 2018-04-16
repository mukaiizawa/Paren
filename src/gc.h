// garbage collector

#define GC_NEW 0
#define GC_OLD 1
#define GC_PERM 2

extern struct xarray object_table;
extern struct xsplay symbol_table;

extern object gc_new_xint(int val);
extern object gc_new_xfloat(double val);
extern object gc_new_cons(object car, object cdr);
extern object gc_new_symbol(char *name);
extern object gc_new_barray(int len);
extern object gc_new_fbarray(int len);

extern void gc_regist(object o);
extern void gc_full(void);
extern void gc_init(void);
