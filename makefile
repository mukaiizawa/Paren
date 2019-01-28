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

paren=paren$(exe)
all: $(paren) core.wk
	./paren -f core.wk

special=ip.c
special.wk: $(special)
	cat $+ | grep ^SPECIAL>$@

prim=bi.c os.c
prim.wk: $(prim)
	cat $+ | grep ^PRIM>$@

xc.a: std.o xarray.o xbarray.o xsplay.o xgetopt.o gc.o object.o lex.o pf.o \
	$(special:%.c=%.o) $(prim:%.c=%.o) 
	ar -ru xc.a $+

$(paren): paren.o xc.a
	$(link)

core.wk: core.p
	cat $+ | sed '/^(assert/d' | sed '/^;/d' >$@
	cat $+ | sed '/^(assert/!d' >>$@

man:
	cat core.p | grep ';:' | cut -c 4- > core.man

clean:
	rm -f *.o *.a *.wk *.man $(paren)

include cdep.d
