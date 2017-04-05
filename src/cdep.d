ast.o: ast.c  ast.h
ast_t$(exe): ast_t.c ast.o ast.h
	$(CC) -o ast_t$(exe) ast_t.c ast.o
	$(pref)ast_t$(exe)
lex.o: lex.c  ast.h lex.h
paren.o: paren.c 

