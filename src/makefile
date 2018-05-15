# makefile
# Usage: make os={linux|windows} [cc=gcc] [debug=off]

supportos=linux windows
cc?=gcc
debug?=on

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
link+= ; $(strip) $(sflags) $@
endif

.SUFFIXES: .c .o
.c.o:
	$(cc) $(cflags) $<

all: $(paren) core.p

special=ip.c
special.wk: $(special)
	cat $+ | grep ^SPECIAL>$@

prim=xint.c list.c
prim.wk: $(prim)
	cat $+ | grep ^PRIM>$@

xc.a: std.o xarray.o xbarray.o xsplay.o xgetopt.o gc.o object.o prim.o lex.o \
	ip.o $(prim:%.c=%.o)
	ar -ru xc.a $+

paren=paren$(exe)
$(paren): paren.o pprim.o xc.a
	$(link)

core.p: $(paren)
	./paren core.p

clean:
	rm -f *.o *.a *.wk $(paren)

include cdep.d
