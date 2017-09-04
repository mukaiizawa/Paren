// paren common header.

typedef union om *object;

union om {
  struct {
    int age;
    object type;
  } header;
  union {
    struct {
      object car, cdr;
    } cons;
    struct {
      char *name;
    } symbol;
    struct {
      int size;
      char elt[1];
    } fbarray;
    struct {
      int size;
      object elt[1];
    } farray;
    struct {
      double val;
    } number;
    struct {
      object params;
      object body;
    } closure;
  } body;
};
