ahdrd.o: ahdrd.c ringbuf.h ahdrd.h
env.o: env.c env.h splay.h prim.h
lex.o: lex.c ringbuf.h ahdrd.h lex.h splay.h prim.h
paren.o: paren.c env.h ringbuf.h ahdrd.h lex.h splay.h prim.h
prim.o: prim.c splay.h prim.h
ringbuf.o: ringbuf.c ringbuf.h
splay.o: splay.c splay.h

