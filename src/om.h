// object memory

#include "xarray.h"

typedef union om *object;

union om {

  int header;

  struct fbarray {
    int header;
    int size;
    char elt[1];
  } fbarray;

  struct farray {
    int header;
    int size;
    object elt[1];
  } farray;

  struct number {
    int header;
    double val;
  } number;

};

extern struct xarray om_table;
extern object om_nil;
extern object om_true;
extern object om_false;
extern object om_primitiveFailed;
extern object om_error;
extern object om_trap_cp_sp;
extern object om_equal;
extern object om_plus;
extern object om_lt;
extern object om_inc;
extern object om_dec;

extern void om_init(void);
