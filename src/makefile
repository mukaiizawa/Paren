# makefile
# Usage: make [os=(linux|windows)] [cc=(clang|gcc)]

supportos=linux windows
os?=linux
cc?=clang
CC=$(cc)
CFLAGS=-Wall

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
objs=paren.o std.o gc.o env.o writer.o reader.o splay.o ahdrd.o array.o \
		 stack.o ringbuf.o

all: $(paren)

$(paren): $(objs)
	$(CC) $(CFLAGS) -g -o $@ $(objs)

include cdep.d

clean:
	$(RM) *.o *.exe
