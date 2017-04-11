# makefile
# make [os=(linux | windows)] [CC=(clang | gcc)]

supportos=linux windows
os ?=linux
CC=clang

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
objs= paren.c lex.o ast.o ahdrd.o ringbuf.o pprim.o

all: $(paren)

$(paren): paren.c $(objs)
	$(CC) -o $@ $(objs)

include cdep.d

clean:
	$(RM) *.o *.exe *_t$(exe)
