// garbage collector

extern object gc_object_new(object xclass,int ext);
extern void gc_refer(object from,object to);
extern void gc_regist_refnew(object o);
extern int gc_mark(object o);
extern void gc_full(void);
extern void gc_chance(void);
extern void gc_init(void);
extern object gc_string(char *s);
