ip.o: ip.c ip.h prim.h object.h xarray.h xsplay.h std.h
lex.o: lex.c xbarray.h lex.h std.h
list.o: list.c
object.o: object.c pprim.wk object.h xbarray.h lex.h xsplay.h xarray.h std.h
paren.o: paren.c pprim.wk ip.h xbarray.h lex.h object.h xarray.h xsplay.h std.h
pprim.o: pprim.c pprim.wk object.h xsplay.h std.h
prim.o: prim.c pprim.wk std.h
std.o: std.c std.h
xarray.o: xarray.c xarray.h std.h
xbarray.o: xbarray.c xbarray.h std.h
xint.o: xint.c prim.h object.h std.h
xsplay.o: xsplay.c xsplay.h std.h
