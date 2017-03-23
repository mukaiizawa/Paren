ast.o: ast.c ast.h
lex.o: lex.c lex.h ringbuf.h
paren.o: paren.c lex.h
ringbuf.o: ringbuf.c std.h ringbuf.h
std.o: std.c std.h
