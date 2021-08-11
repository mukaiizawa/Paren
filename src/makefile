# makefile
# Usage: make os={unix|windows} [cc=gcc] [debug={off|on}]
# 	[sock=off|on]

supportos=unix windows
cc?=gcc
debug?=off
sock?=off

ifeq ($(filter $(supportos),$(os)),)
$(error illegal os)
endif
ifeq ($(os), unix)
extobj+=pf.u.o xsleep.u.o
lib+=-lm
lib+=-ldl
ifeq ($(sock),on)
defun+=sock.c
endif
endif
ifeq ($(os), windows)
extobj+=pf.w.o xsleep.w.o
exe=.exe
ifeq ($(sock),on)
lib+=-lws2_32
defun+=sock.c
endif
endif

uflags=-Wall -Werror
cflags=$(uflags) -c
lflags=$(uflags)
sflags=-s
link=$(cc) $(lflags) -o $@ $+ $(lib)

ifeq ($(debug),on)
uflags+=-g
else
cflags+=-O3 -DNDEBUG
link+= ; strip $(sflags) $@
endif

.SUFFIXES: .c .o
.c.o:
	$(cc) $(cflags) $<

paren=../paren$(exe)
all: $(paren)

defsp=ip.c
defsp.wk: $(defsp)
	cat $+ | grep ^DEFSP>$@

defun+=ip.c bi.c os.c dl.c
defun.wk: $(defun)
	cat $+ | grep ^DEFUN>$@

xc.a: std.o xarray.o xbarray.o heap.o object.o gc.o lex.o st.o \
	$(defsp:%.c=%.o) \
	$(defun:%.c=%.o) \
	$(extobj)
	rm -f $@
	ar -r $@ $+

$(paren): paren.o xc.a
	$(link)

cdep:
	paren cdep.p > make.d

rw.vim:
	paren rw.p | \
		sed 's/|/\\|/g' | \
		sed '/^\[/d' | \
		sed '/^[A-Z]/d' | \
		sed 's/^/syn keyword ParenBuiltIn /' > rw.wk

clean:
	rm -f *.o *.out *.a *.wk $(paren)

include make.d
