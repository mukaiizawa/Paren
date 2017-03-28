ahdrd.o: ahdrd.c std.h
ast.o: ast.c ast.h
lex.o: lex.c lex.h ahdrd.h
paren.o: paren.c lex.h
std.o: std.c std.h
