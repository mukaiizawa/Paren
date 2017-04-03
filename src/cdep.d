ahdrd.o: ahdrd.c ahdrd.h
ast.o: ast.c ast.h
lex.o: lex.c ahdrd.h lex.h
paren.o: paren.c lex.h
queue.o: queue.c queue.h
queue_t: queue_t.c queue.h
	$(CC) -o queue_t$(exe) queue_t.c queue.o
	$(pref)queue_t$(exe)

