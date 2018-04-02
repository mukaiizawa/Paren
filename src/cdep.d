lex.o: lex.c xbarray.h lex.h std.h
object.o: object.c object.h xbarray.h lex.h xsplay.h std.h
paren.o: paren.c prim.wk prim_name.wk xbarray.h lex.h object.h xsplay.h std.h
prim.o: prim.c
prim_name.o: prim_name.c
special.o: special.c
std.o: std.c std.h
xbarray.o: xbarray.c xbarray.h std.h
xsplay.o: xsplay.c xsplay.h std.h
