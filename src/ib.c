// image builder
 
#include "std.h"

#include <stdio.h>
#include <string.h>

#include "splay.h"
#include "heap.h"
#include "ma.h"

#include "om.h"

static struct splay global_splay;
static struct splay symbol_splay;

static int symbol_cmp(struct fbarray *p, struct fbarray *q)
{
  int i, d;
  if(p->size != q->size) return p->size-q->size;
  for(i = 0; i < p->size; i++)
    if((d = p->elt[i] - q->elt[i]) != 0) return d;
  return 0;
}

static void make_initial_objects(void)
{
}

int main(int argc, char *argv[])
{
  setbuf(stdout,NULL);
  om_init();
  splay_init(&symbol_splay,(int(*)(void*,void*))strcmp);
  splay_init(&global_splay,(int(*)(void*,void*))symbol_cmp);
  make_initial_objects();
  printf("%d\n", 3);
  return 0;
}
