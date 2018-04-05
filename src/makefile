# makefile
# Usage: make os={linux|windows} [cc=gcc] [debug=off]

supportos=linux windows
cc?=clang
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

paren=paren$(exe)

.SUFFIXES: .c .o
.c.o:
	$(cc) $(cflags) $<

all: $(paren)

prim=special.c list.c
prim.wk: $(prim)
	cat $+>$@

prim_name.wk: $(prim)
	cat $+ | grep ^PRIM>$@

xc.a: std.o xarray.o xbarray.o xsplay.o object.o lex.o ip.o
	ar -ru xc.a $+

$(paren): paren.o xc.a
	$(link)

clean:
	rm -f *.o *.a *.exe *.wk

include cdep.d
