ahdrd.o: ahdrd.c  ringbuf.h ahdrd.h
ahdrd_t$(exe): ahdrd_t.c ahdrd.o ringbuf.h ahdrd.h
	$(CC) -o ahdrd_t$(exe) ahdrd_t.c ringbuf.o ahdrd.o
	$(pref)ahdrd_t$(exe)
ast.o: ast.c  env.h prim.h ast.h
ast_t$(exe): ast_t.c ast.o env.h prim.h ast.h
	$(CC) -o ast_t$(exe) ast_t.c env.o prim.o ast.o
	$(pref)ast_t$(exe)
env.o: env.c  env.h
env_t$(exe): env_t.c env.o env.h
	$(CC) -o env_t$(exe) env_t.c env.o
	$(pref)env_t$(exe)
lex.o: lex.c  ringbuf.h ahdrd.h env.h prim.h ast.h lex.h
paren.o: paren.c  env.h prim.h ast.h lex.h
prim.o: prim.c  env.h ast.h prim.h
ringbuf.o: ringbuf.c  ringbuf.h
ringbuf_t$(exe): ringbuf_t.c ringbuf.o ringbuf.h
	$(CC) -o ringbuf_t$(exe) ringbuf_t.c ringbuf.o
	$(pref)ringbuf_t$(exe)

