bb.o: bb.c bi.h gc.h object.h xsplay.h config.h std.h
bi.o: bi.c prim.wk special.wk bi.h gc.h object.h xbarray.h xsplay.h config.h std.h
gc.o: gc.c gc.h ip.h xbarray.h lex.h object.h xarray.h xsplay.h config.h std.h
ip.o: ip.c ip.h bi.h gc.h object.h xarray.h xsplay.h config.h std.h
lex.o: lex.c xbarray.h lex.h config.h std.h
object.o: object.c object.h xsplay.h xbarray.h xarray.h config.h std.h
os.o: os.c pf.h bi.h gc.h object.h xsplay.h xbarray.h config.h std.h
paren.o: paren.c bi.h ip.h gc.h xbarray.h lex.h object.h xarray.h xsplay.h xgetopt.h config.h std.h
pf.o: pf.c pf.h xbarray.h config.h std.h
std.o: std.c config.h std.h
xarray.o: xarray.c xarray.h config.h std.h
xbarray.o: xbarray.c xbarray.h config.h std.h
xgetopt.o: xgetopt.c xgetopt.h config.h std.h
xsplay.o: xsplay.c xsplay.h config.h std.h
