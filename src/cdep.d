ahdrd.o: ahdrd.c std.h ahdrd.h
ast.o: ast.c ast.h
lex.o: lex.c lex.h ahdrd.h
paren.o: paren.c lex.h
queue.o: queue.c queue.h std.h
queue_t: queue_t.c queue.h
	$(CC) -o queue_t$(exe) queue_t.c  queue.h std.h
	queue_t$(exe)
std.o: std.c std.h

