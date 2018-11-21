// array/byte array

#include "std.h"
#include "xsplay.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

// TODO after arrengement of number concept
// sint, lint, xfloat

//
// PRIM(barray_new)
// {
//   object x;
//   if (argc != 1 || !typep((x = argv->cons.car), XINT)) return FALSE;
//   *result = gc_new_barray(x->xint.val);
//   return TRUE;
// }
//
// PRIM(array_new)
// {
//   object x;
//   if (argc != 1 || !typep((x = argv->cons.car), XINT)) return FALSE;
//   *result = gc_new_barray(x->xint.val);
//   return TRUE;
// }
//
// // refer/assign
// PRIM(array_access)
// {
//   int i, x;
//   object a, k, v;
//   if (argc < 2 || argc > 3) return FALSE;
//   a = argv->cons.car;
//   k = (argv = argv->cons.cdr)->cons.car;
//   if (!numberp(k)) return FALSE;
//   switch (argv->cons.car) {
//     case BARRAY:
//       if (k < 0 || a->barray.size >= k) return FALSE;
//       if (argc == 2) *result = gc_new_xint(a->barray.elt[k]);
//       else {
//         v = argv->cons.cdr->cons.car;
//         if (!typep(v, XINT) || )
//         a->barray.elt[k] = 
//           *result = argv
//       }
//       return TRUE;
//
//     // case ARRAY:
//     default:
//       return FALSE;
//   }
// }
