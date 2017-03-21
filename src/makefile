# makefile
# make [os = (linux | windows)] [cc = (clang | gcc)]

supportos = linux windows
supportcc = clang gcc
os ?= linux
cc ?= clang

ifeq ($(filter $(supportcc),$(cc)),)
	$(error Unsupport compiler name.)
endif

ifeq ($(filter $(supportos),$(os)),)
	$(error Unsupport os.)
endif

ifeq ($(os), windows)
	exe = .exe
endif

paren = paren$(exe)
link = $(cc) -Wall -O2

all: $(paren)

$(paren): lex.o
	$(link) -o $@ paren.c lex.o

lex.o: lex.h lex.c
	$(link) -c lex.c -o lex.o

clean:
	rm -f *.o
