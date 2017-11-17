// object memory

#include "xarray.h"

typedef union om *object;

#define HEADER int header;\
  object type\

union om {

  HEADER;

  struct fbarray {
    HEADER;
    int size;
    char elt[1];
  } fbarray;

  struct farray {
    HEADER;
    int size;
    object elt[1];
  } farray;

  struct xint {
    HEADER;
    int val;
  } xint;

  struct xfloat {
    HEADER;
    double val;
  } xfloat;

  struct cons {
    HEADER;
    object car, cdr;
  } cons;

  struct func {
    HEADER;
    object args;
    object bytecode;
  } func;

  struct symbol {
    HEADER;
    object args;
    object bytecode;
  } symbol;

};

extern struct xarray om_table;
extern object om_nil;
extern object om_true;
extern object om_false;
extern object om_error;
extern object om_trap_cp_sp;

extern void om_init(void);
