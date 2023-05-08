# makefile
# Usage: make os={unix|windows} [cc=gcc] [debug={off|on}]
# 	[sock=off|on] [clip=off|on] [mouse=off|on] [keyboard=off|on]

supportedos=unix windows
ifeq ($(filter $(supportedos),$(os)),)
$(error illegal os)
endif

ifeq ($(os), unix)
cc?=gcc
sock?=on
clip?=off
mouse?=off
keyboard?=off
obj+=pf.u.o xiconv.u.o xsleep.u.o
ifeq ($(sock),on)
obj+=sock.o
endif
ifeq ($(clip),on)
$(error clip does not supported)
endif
ifeq ($(mouse),on)
$(error mouse does not supported)
endif
ifeq ($(keyboard),on)
$(error keyboard does not supported)
endif
lib+=-lm
endif

ifeq ($(os), windows)
cc?=x86_64-w64-mingw32-gcc
sock?=on
clip?=on
mouse?=on
keyboard?=on
obj+=pf.w.o xiconv.w.o xsleep.w.o
exe=.exe
ifeq ($(sock),on)
obj+=sock.o
dl_ws?=-lws2_32
endif
ifeq ($(clip),on)
obj+=clip.o
dl_user?=-lUser32
dl_ws?=-lws2_32
endif
ifeq ($(mouse),on)
obj+=mouse.o
dl_user?=-lUser32
dl_ws?=-lws2_32
endif
ifeq ($(keyboard),on)
obj+=keyboard.o
dl_user?=-lUser32
dl_ws?=-lws2_32
endif
lib+=$(dl_user)
lib+=$(dl_ws)
endif

obj+=std.o xarray.o xbarray.o heap.o om.o lex.o ip.o bi.o xiconv.o paren.o
uflags=-Wall -Werror
cflags=$(uflags) -c
lflags=$(uflags)
link=$(cc) $(lflags) -o $@ $+ $(lib)

debug?=off
ifeq ($(debug),on)
uflags+=-g
else
cflags+=-O3 -DNDEBUG
link+=; strip -s $@
endif

.SUFFIXES: .c .o
.c.o:
	$(cc) $(cflags) $<

paren=../paren$(exe)
all: $(paren)

defsp.wk: $(obj:%.o=%.c)
	cat $+ | grep ^DEFSP > $@

defun.wk: $(obj:%.o=%.c)
	cat $+ | grep ^DEFUN > $@

$(paren): $(obj)
	$(link)

punit: $(paren)
	$(paren) manunit
	$(paren) punit -r ./wk

mandb: $(paren)
	$(paren) mandb

cdep: $(paren)
	$(paren) cdep > make.d

rw.vim: $(paren)
	$(paren) rw | sort | sed -e 's/|/\\|/g' -e '/\[/d' -e 's/^/sy keyword ParenBuiltIn /' > rw.wk

clean:
	rm -rf wk *.wk *.o *.out *.a $(paren)

include make.d
