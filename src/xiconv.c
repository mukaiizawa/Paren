// iconv.

#include "std.h"
#include "xiconv.h"
#include "om.h"
#include "bi.h"
#include "ip.h"

static int om_to_encoding(object o, int *enc)
{
  if (o == om_new_mem_from_cstr(SYMBOL, ":sjis")) *enc = XICONV_SJIS;
  else if (o == om_new_mem_from_cstr(SYMBOL, ":utf8")) *enc = XICONV_UTF8;
  else return ip_sigerr(ArgumentError, "unsupported encoding");
  return TRUE;
}

DEFUN(iconv_2e_encode)
{
  int from, to;
  char *s0, *s1;
  from = to = XICONV_UTF8;
  if (!bi_argc_range(argc, 2, 3)) return FALSE;
  if (!bi_cstring(argv->cons.car, &s0)) return FALSE;
  if (!om_to_encoding((argv = argv->cons.cdr)->cons.car, &to)) return FALSE;
  if (argc == 3) {
    from = to;
    if (!om_to_encoding(argv->cons.cdr->cons.car, &to)) return FALSE;
  }
  if (!xiconv(from, to, s0, &s1)) return FALSE;
  *result = om_new_mem_from_cstr(STRING, s1);
  return TRUE;
}
