/*
  garbage collector.
*/

#define GC_MAX_NEW_NUM 100
#define GC_OLD_FREQ 5

#define GC_OLD 0
#define GC_NEW 3
#define GC_PERM 4

typedef struct {
  S *newList[GC_MAX_NEW_NUM];
} GC;

extern void GC_init(GC *gc);
extern void GC_regist(GC *gc, S *expr);
