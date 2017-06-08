/*
  garbage collector.
*/

#define GC_MAX_NEWBIE_NUM 100
#define GC_FREQ_ELDER 5

#define GC_NEWBIE 0
#define GC_ELDER 3
#define GC_PERM 4

typedef struct {
  int count;
  S *newbies[GC_MAX_NEWBIE_NUM];
  // elders
} GC;

extern void GC_init(GC *gc);
extern void GC_regist(GC *gc, S *expr);
