/*
  garbage collector.
*/

#define GC_NEWBIE_SIZE 100
#define GC_INIT_ELDER_SIZE 500
#define GC_POOL_SIZE 500

#define GC_FREQ_ELDER 5

#define GC_NEWBIE 0
#define GC_ELDER 3
#define GC_PERM 4

typedef struct {
  int times;
  Array newbies, elders;
  Stack pool;
} GC;

extern void GC_init(GC *gc);
extern void GC_try(GC *gc);
extern S *GC_alloc(GC *gc);
