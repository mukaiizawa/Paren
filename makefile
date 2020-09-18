# makefile
# Usage: make os={linux|windows} [cc=gcc] [debug={off|on}]

supportos=linux windows
cc?=gcc
debug?=off

ifeq ($(filter $(supportos),$(os)),)
$(error illegal os)
endif
ifeq ($(os), linux)
extobj+=pf.u.o xsleep.u.o
endif
ifeq ($(os), windows)
extobj+=pf.w.o xsleep.w.o
lib+=-lws2_32
exe=.exe
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

paren=paren$(exe)
all: $(paren)

defsp=ip.c
defsp.wk: $(defsp)
	cat $+ | grep ^DEFSP>$@

defun=ip.c bi.c array.c bytes.c cons.c lambda.c number.c os.c dl.c sock.c
defun.wk: $(defun)
	cat $+ | grep ^DEFUN>$@

xc.a: std.o xarray.o xbarray.o xgetopt.o heap.o object.o gc.o lex.o splay.o st.o \
	$(defsp:%.c=%.o) \
	$(defun:%.c=%.o) \
	$(extobj)
	ar -ru xc.a $+

$(paren): paren.o xc.a
	$(link)

cdep:
	paren cdep.p > make.d

rw.vim:
	paren rw.p | \
		sed 's/|/\\|/g' | \
		sed '/^\[/d' | \
		sed '/^[A-Z]/d' | \
		sed 's/^/syn keyword ParenBuiltin /' > rw.wk

clean:
	rm -f *.o *.out *.a *.wk $(paren)

include make.d
