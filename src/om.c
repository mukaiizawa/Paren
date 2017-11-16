// object memory

#include "std.h"
#include "ma.h"
#include "heap.h"
#include "om.h"

static int hash_count;
static struct heap heap;
#define LINK0_SIZE (8*sizeof(intptr_t))
#define LINK1_SIZE (LINK0_SIZE*2)
static object link0;
static object link1;

struct xarray om_table;

int om_used_memory;
int om_max_used_memory;

void om_init(void)
{
  xarray_init(&om_table);
  hash_count=0;
  om_used_memory=0;
  om_max_used_memory=0;
  heap_init(&heap);
  link0=NULL;
  link1=NULL;
}
