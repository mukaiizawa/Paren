# makefile
# Usage: make [os=(linux|windows)] [cc=(clang|gcc)]

supportos=linux windows
os?=linux
cc?=clang
CC=$(cc)

ifeq ($(filter $(supportos),$(os)),)
	$(error Unsupport os.)
endif

ifeq ($(os), linux)
	pref=./
endif

ifeq ($(os), windows)
	exe=.exe
endif

paren=paren$(exe)
objs=paren.c prim.o lex.o env.o ahdrd.o ringbuf.o

all: $(paren)

$(paren): paren.c $(objs)
	$(CC) -Wall -o $@ $(objs)

include cdep.d

clean:
	$(RM) *.o *.exe
