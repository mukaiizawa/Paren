ip.o: ip.c
lex.o: lex.c xbarray.h lex.h std.h
list.o: list.c
object.o: object.c prim_name.wk object.h xbarray.h lex.h xsplay.h xarray.h std.h
paren.o: paren.c prim_name.wk ip.h xbarray.h lex.h object.h xarray.h xsplay.h std.h
prim.o: prim.c prim_name.wk
special.o: special.c prim.h object.h std.h
std.o: std.c std.h
xarray.o: xarray.c xarray.h std.h
xbarray.o: xbarray.c xbarray.h std.h
xsplay.o: xsplay.c xsplay.h std.h
