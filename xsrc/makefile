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

paren=paren$(exe)
all: $(paren)

objs=paren.o std.o gc.o env.o writer.o reader.o splay.o ahdrd.o array.o \
		 stack.o ringbuf.o
$(paren): $(objs)
	$(CC) $(CFLAGS) -g -o $@ $(objs)

include cdep.d

clean:
	$(RM) *.o *.exe
