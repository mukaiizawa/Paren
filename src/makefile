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

ib=ib$(exe)

.SUFFIXES: .c .o
.c.o:
	$(cc) $(cflags) $<

all: $(ib)

xc.a: std.o heap.o xarray.o om.o splay.o
	ar -ru xc.a $+

$(ib): ib.o xc.a
	$(link)

clean:
	rm -f *.o *.exe

include cdep.d
