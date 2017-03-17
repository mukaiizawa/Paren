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

paren: paren.c
	$(cc) -Wall -O2 -o paren$(exe) paren.c
