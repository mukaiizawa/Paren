# makefile
# make [os = (linux | windows)] [CC = (clang | gcc)]

supportos = linux windows
os ?= linux
CC = clang

ifeq ($(filter $(supportos),$(os)),)
$(error Unsupport os.)
endif

ifeq ($(os), windows)
exe = .exe
endif

paren = paren$(exe)

all: $(paren)

$(paren): paren.c lex.o std.o ahdrd.o
	$(CC) -o $@ paren.c lex.o std.o ahdrd.o

include cdep.d

clean:
	$(RM) *.o
