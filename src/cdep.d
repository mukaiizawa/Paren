ahdrd.o: ahdrd.c ringbuf.h ahdrd.h std.h
env.o: env.c env.h splay.h std.h
lex.o: lex.c ringbuf.h ahdrd.h lex.h prim.h env.h splay.h
paren.o: paren.c ringbuf.h ahdrd.h lex.h prim.h env.h splay.h
prim.o: prim.c prim.h env.h splay.h std.h
ringbuf.o: ringbuf.c ringbuf.h
splay.o: splay.c splay.h std.h
std.o: std.c std.h

