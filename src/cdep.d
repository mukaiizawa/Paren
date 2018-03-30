lex.o: lex.c xbarray.h lex.h std.h
object.o: object.c object.h xbarray.h lex.h xsplay.h std.h
paren.o: paren.c prim.wk xbarray.h lex.h object.h xsplay.h std.h
special.o: special.c prim.h object.h std.h
std.o: std.c std.h
xbarray.o: xbarray.c xbarray.h std.h
xsplay.o: xsplay.c xsplay.h std.h
