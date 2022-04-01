// iconv.

#include "std.h"
#include "xiconv.h"
#include "object.h"
#include "gc.h"
#include "bi.h"

static int object_to_encoding(object o, int *enc)
{
  object p;
  char buf[MAX_STR_LEN];
  if (!bi_argv(BI_KEY, o, &p)) return FALSE;
  if (p->mem.size >= MAX_STR_LEN) return FALSE;
  memcpy(buf, p->mem.elt, p->mem.size);
  buf[p->mem.size] = '\0';
  *enc =XICONV_UNSUPPORTED;
  if (strcmp(buf, "sjis") == 0) *enc = XICONV_SJIS;
  if (strcmp(buf, "utf8") == 0) *enc = XICONV_UTF8;
  return *enc != XICONV_UNSUPPORTED;
}

DEFUN(iconv_2e_encode)
{
  int enc0, enc1;
  char *s0, *s1;
  if (!bi_argc_range(argc, 2, 3)) return FALSE;
  if (!bi_cstring(argv, &s0)) return FALSE;
  if (!object_to_encoding((argv = argv->cons.cdr)->cons.car, &enc0)) return FALSE;
  if (argc == 2) {
    enc1 = enc0;
    enc0 = XICONV_UTF8;
  } else if (!object_to_encoding(argv->cons.cdr->cons.car, &enc1)) return FALSE;
  if (!xiconv(enc0, enc1, s0, &s1)) return FALSE;
  *result = gc_new_mem_from(STRING, s1, strlen(s1));
  return TRUE;
}
