# makefile
# Usage: make os={linux|windows} [cc=gcc] [debug=on]

supportos=linux windows
cc?=gcc
debug?=off

ifeq ($(filter $(supportos),$(os)),)
$(error Unsupport os)
endif
ifeq ($(os), linux)
endif
ifeq ($(os), windows)
exe=.exe
endif

uflags=-Wall -Werror
cflags=$(uflags) -c
lflags=$(uflags)
sflags=-s
link=$(cc) $(lflags) -o $@ $+ $(libs)

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

defun=ip.c bi.c array.c barray.c cons.c keyword.c lambda.c number.c string.c symbol.c os.c
defun.wk: $(defun)
	cat $+ | grep ^DEFUN>$@

xc.a: std.o xsleep.o xarray.o xbarray.o xgetopt.o heap.o pf.o object.o gc.o lex.o splay.o st.o \
	$(defsp:%.c=%.o) \
	$(defun:%.c=%.o) 
	ar -ru xc.a $+

$(paren): paren.o xc.a
	$(link)

prof:
	gprof $(paren) > prof.wk

clean:
	rm -f *.o *.out *.a *.wk $(paren)

include cdep.d
