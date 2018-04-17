gc.o: gc.c xbarray.h lex.h object.h xarray.h xsplay.h std.h
ip.o: ip.c ip.h prim.h gc.h object.h xarray.h xsplay.h std.h
lex.o: lex.c xbarray.h lex.h std.h
object.o: object.c object.h xbarray.h lex.h xsplay.h xarray.h std.h
paren.o: paren.c ip.h gc.h xbarray.h lex.h object.h xarray.h xsplay.h xgetopt.h std.h
pprim.o: pprim.c pprim.wk object.h xsplay.h std.h
prim.o: prim.c pprim.wk prim.h object.h xsplay.h std.h
std.o: std.c std.h
xarray.o: xarray.c xarray.h std.h
xbarray.o: xbarray.c xbarray.h std.h
xgetopt.o: xgetopt.c xgetopt.h std.h
xint.o: xint.c prim.h object.h xsplay.h std.h
xsplay.o: xsplay.c xsplay.h std.h
