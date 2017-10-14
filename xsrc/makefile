# makefile
# Usage: make [os=(linux|windows)] [cc=(clang|gcc)] [debug=off]

supportos=linux windows
os?=linux
cc?=clang
debug?=off
uflags=-Wall -Werror
cflags=$(uflags) -c
lflags=$(uflags)
libs=
link=$(cc) $(lflags) -o $@ $+ $(libs)

ifeq ($(filter $(supportos),$(os)),)
	$(error Unsupport os.)
endif

ifeq ($(os), linux)
	exe=
endif

ifeq ($(os), windows)
	exe=.exe
endif

ifeq ($(debug),on)
uflags+=-g
else
cflags+=-O3 -DNDEBUG
link+= && $(strip) $(sflags) $@
endif

.SUFFIXES: .c .o

.c.o:
	$(cc) $(cflags) $<

all: $(paren) paren.mi

xc.a: std.o heap.o xbarray.o xctype.o splay.o xgetopt.o log.o xarray.o pf.o \
	cqueue.o xsleep.o
	ar -ru xc.a $+

paren.mi: xc.a
	echo should be implemented.

include cdep.d

clean:
	rm -f *.o *.a *.wk
	rm -f $(paren)
