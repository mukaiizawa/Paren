ahdrd.o: ahdrd.c  ringbuf.h ahdrd.h
ahdrd_t$(exe): ahdrd_t.c ahdrd.o ringbuf.h ahdrd.h
	$(CC) -o ahdrd_t$(exe) ahdrd_t.c ringbuf.o ahdrd.o
	$(pref)ahdrd_t$(exe)
ast.o: ast.c  ast.h
ast_t$(exe): ast_t.c ast.o ast.h
	$(CC) -o ast_t$(exe) ast_t.c ast.o
	$(pref)ast_t$(exe)
lex.o: lex.c  ringbuf.h ahdrd.h ast.h lex.h
paren.o: paren.c 
ringbuf.o: ringbuf.c  ringbuf.h
ringbuf_t$(exe): ringbuf_t.c ringbuf.o ringbuf.h
	$(CC) -o ringbuf_t$(exe) ringbuf_t.c ringbuf.o
	$(pref)ringbuf_t$(exe)

