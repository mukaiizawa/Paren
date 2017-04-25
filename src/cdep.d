ahdrd.o: ahdrd.c  ringbuf.h ahdrd.h
ahdrd_t$(exe): ahdrd_t.c ahdrd.o ringbuf.h ahdrd.h
	$(CC) -o ahdrd_t$(exe) ahdrd_t.c ringbuf.o ahdrd.o
	$(pref)ahdrd_t$(exe)
env.o: env.c  env.h prim.h
env_t$(exe): env_t.c env.o env.h prim.h lex.h
	$(CC) -o env_t$(exe) env_t.c env.o prim.o
	$(pref)env_t$(exe)
lex.o: lex.c  ringbuf.h ahdrd.h env.h prim.h lex.h
paren.o: paren.c  env.h prim.h lex.h
prim.o: prim.c  lex.h env.h prim.h macro.h
ringbuf.o: ringbuf.c  ringbuf.h
ringbuf_t$(exe): ringbuf_t.c ringbuf.o ringbuf.h
	$(CC) -o ringbuf_t$(exe) ringbuf_t.c ringbuf.o
	$(pref)ringbuf_t$(exe)

