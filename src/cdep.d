ahdrd.o: ahdrd.c ringbuf.h ahdrd.h
env.o: env.c env.h prim.h
lex.o: lex.c ringbuf.h ahdrd.h env.h prim.h lex.h
paren.o: paren.c env.h prim.h lex.h
prim.o: prim.c lex.h env.h prim.h macro.h
ringbuf.o: ringbuf.c ringbuf.h

