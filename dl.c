// dynamic library

#include "std.h"

#if UNIX_P
#include <dlfcn.h>
#define DLOPEN(n) ((intptr_t)dlopen(n, RTLD_LAZY))
#define DLSYM(h, n) ((intptr_t)dlsym((void*)h, n))
#endif

#if WINDOWS_P
#define DLOPEN(n) ((intptr_t)LoadLibrary(n))
#define DLSYM(h, n) ((intptr_t)GetProcAddress((HMODULE)h, n))
#endif

#include "object.h"
#include "gc.h"
#include "bi.h"

DEFUN(dl_open)
{
  char *n;
  intptr_t h;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if ((n = bi_string(argv->cons.car)) == NULL) return FALSE;
  if ((h = DLOPEN(n)) == 0) return FALSE;
  *result = gc_new_xint(h);
  return TRUE;
}

DEFUN(dl_sym)
{
  char *n;
  intptr_t h, sym;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_intptr(argv->cons.car, &h)) return FALSE;
  if ((n = bi_string(argv->cons.cdr->cons.car)) == NULL) return FALSE;
  if ((sym = DLSYM(h, n)) == 0) return FALSE;	
  *result = gc_new_xint(sym);
  return TRUE;
}

#define MAX_ARGS 7

DEFUN(dl_call)
{
  int i, type;
  intptr_t func, cargs[MAX_ARGS], cret;
  object fargs;
  if (!bi_argc_range(argc, 3, 3)) return FALSE;
  if (!bi_intptr(argv->cons.car, &func)) return FALSE;
  if (!bi_sint((argv = argv->cons.cdr)->cons.car, &type)) return FALSE;
  if (!bi_arg_type(argv->cons.cdr->cons.car, ARRAY, &fargs)) return FALSE;
  cret = 0;
  for (i = 0; i < type % 100; i++) {
    if (!bi_intptr(fargs->array.elt[i], &cargs[i])) return FALSE;
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
    default: return FALSE;
  }
  *result = gc_new_xint(cret);
  return TRUE;
}

DEFUN(dl_load_byte)
{
  intptr_t addr;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_intptr(argv->cons.car, &addr)) return FALSE;
  *result = gc_new_xint(LC(addr));
  return TRUE;
}

DEFUN(dl_store_byte)
{
  int byte;
  intptr_t addr;
  if (!bi_argc_range(argc, 2, 2)) return FALSE;
  if (!bi_intptr(argv->cons.car, &addr)) return FALSE;
  if (!bi_sint(argv->cons.cdr->cons.car, &byte)) return FALSE;
  if (!byte_p(byte)) return FALSE;
  SC(addr, byte);
  return TRUE;
}

DEFUN(mem_address)
{
  object o;
  if (!bi_argc_range(argc, 1, 1)) return FALSE;
  if (!bi_arg_mutable_mem(argv->cons.car, &o)) return FALSE;
  *result = gc_new_xint((intptr_t)o->mem.elt);
  return TRUE;
}
