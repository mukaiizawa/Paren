ahdrd.o: ahdrd.c ringbuf.h ahdrd.h
env.o: env.c env.h prim.h splay.h
lex.o: lex.c ringbuf.h ahdrd.h lex.h prim.h splay.h
paren.o: paren.c env.h splay.h ringbuf.h ahdrd.h lex.h prim.h
prim.o: prim.c prim.h splay.h
ringbuf.o: ringbuf.c ringbuf.h
splay.o: splay.c splay.h

