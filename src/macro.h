/*
  macro utility.
*/

#ifndef IS_LOADED_MACRO
#define IS_LOADED_MACRO

#define defalloc(type) \
  struct type *type ## _alloc() { \
    struct type *obj; \
    if ((obj = (struct type *)calloc(1, sizeof(struct type))) == NULL) { \
      fprintf(stderr, "%s_alloc: Cannot allocate memory.", #type); \
      exit(1); \
    } \
    return obj; \
  }

#endif
