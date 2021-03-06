// dynamic library.

#include "std.h"
#include "object.h"
#include "gc.h"
#include "bi.h"
#include "ip.h"

#if UNIX_P
#define DLOPEN(n) ((intptr_t)dlopen(n, RTLD_LAZY))
#define DLSYM(h, n) ((intptr_t)dlsym((void*)h, n))
#endif

#if WINDOWS_P
#define DLOPEN(n) ((intptr_t)LoadLibrary(n))
#define DLSYM(h, n) ((intptr_t)GetProcAddress((HMODULE)h, n))
#endif

DEFUN(dl_2e_fopen)
{
  char *n;
  intptr_t h;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cstring(argv, &n)) return FALSE;
  if ((h = DLOPEN(n)) == 0) return ip_throw(OSError, builtin_failed);
  *result = gc_new_xint(h);
  return TRUE;
}

DEFUN(dl_2e_sym)
{
  char *n;
  intptr_t h, sym;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cintptr(argv->cons.car, &h)) return FALSE;
  if (!bi_cstring(argv->cons.cdr, &n)) return FALSE;
  if ((sym = DLSYM(h, n)) == 0) return ip_throw(OSError, builtin_failed);
  *result = gc_new_xint(sym);
  return TRUE;
}

#define MAX_ARGS 7

DEFUN(dl_2e_call)
{
  int i, type;
  intptr_t func, cargs[MAX_ARGS], cret;
  object fargs;
  if (!bi_argc_range(argc, 3, 3)) return FALSE;
  if (!bi_cintptr(argv->cons.car, &func)) return FALSE;
  if (!bi_cint((argv = argv->cons.cdr)->cons.car, &type)) return FALSE;
  if (!bi_array(argv->cons.cdr->cons.car, &fargs)) return FALSE;
  cret = 0;
  for (i = 0; i < type % 100; i++) {
    if (!bi_cintptr(fargs->array.elt[i], &cargs[i])) return FALSE;
  }
#define A(i) (cargs[i])
  switch(type) {
#define F (*((intptr_t (*)())func))
    case 0: cret = F(); break;
    case 1: cret = F(A(0)); break;
    case 2: cret = F(A(0), A(1)); break;
    case 3: cret = F(A(0), A(1), A(2)); break;
    case 4: cret = F(A(0), A(1), A(2), A(3)); break;
    case 5: cret = F(A(0), A(1), A(2), A(3), A(4)); break;
    case 6: cret = F(A(0), A(1), A(2), A(3), A(4), A(5)); break;
    case 7: cret = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6)); break;
#undef F
#if WINDOWS_P
#define F (*((intptr_t (__stdcall *)())func))
    case 100: cret = F(); break;
    case 101: cret = F(A(0)); break;
    case 102: cret = F(A(0), A(1)); break;
    case 103: cret = F(A(0), A(1), A(2)); break;
    case 104: cret = F(A(0), A(1), A(2), A(3)); break;
    case 105: cret = F(A(0), A(1), A(2), A(3), A(4)); break;
    case 106: cret = F(A(0), A(1), A(2), A(3), A(4), A(5)); break;
    case 107: cret = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6)); break;
#undef F
#endif
    default: ip_throw(ArgumentError,  invalid_args);
  }
  *result = gc_new_xint(cret);
  return TRUE;
}

DEFUN(dl_2e_load_2d_byte)
{
  intptr_t addr;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_cintptr(argv->cons.car, &addr)) return FALSE;
  *result = gc_new_xint(LC(addr));
  return TRUE;
}

DEFUN(dl_2e_store_2d_byte)
{
  int byte;
  intptr_t addr;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_cintptr(argv->cons.car, &addr)) return FALSE;
  if (!bi_cbyte(argv->cons.cdr->cons.car, &byte)) return FALSE;
  SC(addr, byte);
  return TRUE;
}

DEFUN(dl_2e_address)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_bytes(argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint((intptr_t)o->mem.elt);
  return TRUE;
}
