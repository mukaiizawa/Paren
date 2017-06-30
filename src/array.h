/*
   extensible array.
*/

typedef struct {
  void *contents;
  int size, length;
} Array;

extern void Array_init(Array *arr, int size, int length);
extern void *Array_get(Array *arr, int i);
extern void Array_put(Array *arr, int i, void *val);
extern void Array_extend(Array *arr, int i);
extern void Array_free(Array *arr);
