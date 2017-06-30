/*
   extensible array.
*/

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "std.h"
#include "array.h"

void Array_init(Array *arr, int size, int length) {
  arr->contents = NULL;
  arr->size = size;
  arr->length = 0;
  Array_extend(arr, length);
}

void *Array_get(Array *arr, int i) {
  assert(arr->length > i);
  return (void *)(arr->contents + (i * arr->size));
}

void Array_put(Array *arr, int i, void *val) {
  void *p;
  assert(arr->length > i);
  p = arr->contents + (i * arr->size);
  memcpy(p, val, arr->size);
}

void Array_extend(Array *arr, int i) {
  assert(arr->length < i);
  arr->length = i;
  arr->contents = xrealloc(arr->contents, (i * arr->size));
}

void Array_free(Array *arr) {
  free(arr->contents);
}
