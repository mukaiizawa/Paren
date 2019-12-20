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
uflags+=-pg
else
cflags+=-O3 -DNDEBUG
link+= ; strip $(sflags) $@
endif

.SUFFIXES: .c .o
.c.o:
	$(cc) $(cflags) $<

paren=paren$(exe)
all: $(paren)
	./paren$(exe)

special=ip.c
special.wk: $(special)
	cat $+ | grep ^SPECIAL>$@

prim=ip.c bi.c seq.c number.c lambda.c os.c
prim.wk: $(prim)
	cat $+ | grep ^PRIM>$@

xc.a: std.o xarray.o xbarray.o xgetopt.o heap.o pf.o \
	object.o gc.o lex.o splay.o \
	$(special:%.c=%.o) $(prim:%.c=%.o) 
	ar -ru xc.a $+

$(paren): paren.o xc.a
	$(link)

prof:
	gprof $(paren) > prof.wk

clean:
	rm -f *.o *.out *.a *.wk $(paren)

include cdep.d
