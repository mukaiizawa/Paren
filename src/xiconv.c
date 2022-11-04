// iconv.

#include "std.h"
#include "xiconv.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

static int object_to_encoding(object o, int *enc)
{
  if (o == gc_new_mem_from_cstr(SYMBOL, ":sjis")) *enc = XICONV_SJIS;
  else if (o == gc_new_mem_from_cstr(SYMBOL, ":utf8")) *enc = XICONV_UTF8;
  else return ip_throw(ArgumentError, index_out_of_range);
  return TRUE;
}

DEFUN(iconv_2e_encode)
{
  int from, to;
  char *s0, *s1;
  from = to = XICONV_UTF8;    // supress maybe-uninitialized
  if (!bi_argc_range(argc, 2, 3)) return FALSE;
  if (!bi_cstring(argv, &s0)) return FALSE;
  if (!object_to_encoding((argv = argv->cons.cdr)->cons.car, &to)) return FALSE;
  if (argc == 3) {
    from = to;
    if (!object_to_encoding(argv->cons.cdr->cons.car, &to)) return FALSE;
  }
  if (!xiconv(from, to, s0, &s1)) return FALSE;
  *result = gc_new_mem_from_cstr(STRING, s1);
  return TRUE;
}
