# makefile
# Usage: make os={unix|windows} [cc=gcc] [debug={off|on}]
# 	[sock=off|on] [clip=off|on] [mouse=off|on] [keyboard=off|on]

supportos=unix windows
cc?=gcc
debug?=off

ifeq ($(filter $(supportos),$(os)),)
$(error illegal os)
endif

ifeq ($(os), unix)
sock?=on
clip?=off
mouse?=off
keyboard?=off
extobj+=pf.u.o xiconv.u.o xsleep.u.o
dl_m?=-lm
ifeq ($(sock),on)
defun+=sock.c
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
endif

ifeq ($(os), windows)
sock?=on
clip?=on
mouse?=on
keyboard?=on
extobj+=pf.w.o xiconv.w.o xsleep.w.o
exe=.exe
ifeq ($(sock),on)
dl_ws?=-lws2_32
defun+=sock.c
endif
ifeq ($(clip),on)
dl_user?=-lUser32
dl_ws?=-lws2_32
defun+=clip.c
endif
ifeq ($(mouse),on)
dl_user?=-lUser32
dl_ws?=-lws2_32
defun+=mouse.c
endif
ifeq ($(keyboard),on)
dl_user?=-lUser32
dl_ws?=-lws2_32
defun+=keyboard.c
endif
endif

lib+=$(dl_m)
lib+=$(dl_user)
lib+=$(dl_ws)

uflags=-Wall -Werror
cflags=$(uflags) -c
lflags=$(uflags)
sflags=-s
link=$(cc) $(lflags) -o $@ $+ $(lib)

ifeq ($(debug),on)
uflags+=-g
else
cflags+=-O3 -DNDEBUG
link+= ; strip $(sflags) $@
endif

.SUFFIXES: .c .o
.c.o:
	$(cc) $(cflags) $<

paren=../paren$(exe)
all: $(paren)

defsp=ip.c
defsp.wk: $(defsp)
	cat $+ | grep ^DEFSP>$@

defun+=ip.c bi.c os.c
defun.wk: $(defun)
	cat $+ | grep ^DEFUN>$@

xc.a: std.o xarray.o xbarray.o heap.o object.o gc.o lex.o st.o \
	$(defsp:%.c=%.o) \
	$(defun:%.c=%.o) \
	$(extobj)
	rm -f $@
	ar -r $@ $+

$(paren): paren.o xc.a
	$(link)

cdep:
	paren cdep.p > make.d

rw.vim:
	paren rw.p | \
		sed 's/|/\\|/g' | \
		sed '/^\[/d' | \
		sed '/^[A-Z]/d' | \
		sed 's/^/syn keyword ParenBuiltIn /' > rw.wk

clean:
	rm -f *.o *.out *.a *.wk $(paren)

include make.d
