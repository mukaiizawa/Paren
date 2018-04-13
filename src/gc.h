// garbage collector

#define GC_NEW 0
#define GC_OLD 1
#define GC_PERM 2

extern object gc_new_cons();

extern void gc_regist(object o);
extern void gc_full(void);
extern void gc_init(void);
