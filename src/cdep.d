ahdrd.o: ahdrd.c  ringbuf.h
ast.o: ast.c  ast.h
ast_t$(exe): ast_t.c ast.o ast.h
	$(CC) -o ast_t$(exe) ast_t.c ast.o
	$(pref)ast_t$(exe)
lex.o: lex.c  ast.h lex.h
paren.o: paren.c 
ringbuf.o: ringbuf.c  ringbuf.h
ringbuf_t$(exe): ringbuf_t.c ringbuf.o ringbuf.h
	$(CC) -o ringbuf_t$(exe) ringbuf_t.c ringbuf.o
	$(pref)ringbuf_t$(exe)

