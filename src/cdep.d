ahdrd.o: ahdrd.c ringbuf.h ahdrd.h
env.o: env.c env.h prim.h sexpr.h
lex.o: lex.c ahdrd.h lex.h prim.h sexpr.h
paren.o: paren.c env.h lex.h prim.h sexpr.h
prim.o: prim.c lex.h prim.h sexpr.h
ringbuf.o: ringbuf.c ringbuf.h
sexpr.o: sexpr.c sexpr.h

