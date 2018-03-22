# makefile
# Usage: make [os={linux|windows}] [cc={clang|gcc}] [debug={on|off}]

supportos=linux windows
os?=linux
cc?=clang
debug?=off

ifeq ($(filter $(supportos),$(os)),)
	$(error Unsupport os.)
endif
ifeq ($(os), linux)
endif
ifeq ($(os), windows)
	exe=.exe
endif

ifeq ($(debug), on)
cflags+=-O3 -DNDEBUG
link+= ; $(strip) $(sflags) $@
else
uflags+=-g
endif

uflags=-Wall -Werror
cflags=$(uflags) -c
lflags=$(uflags)
sflags=-s
link=$(cc) $(lflags) -o $@ $+ $(libs)

paren = paren$(exe)

.SUFFIXES: .c .o
.c.o:
	$(cc) $(cflags) $<

all: $(paren)

xc.a: std.o xbarray.o lex.o
	ar -ru xc.a $+

$(paren): paren.o xc.a
	$(link)

clean:
	rm -f *.o *.a *.exe

include cdep.d
