bi.o: bi.c defun.wk defsp.wk ip.h bi.h gc.h object.h xbarray.h xarray.h config.h std.h
gc.o: gc.c gc.h ip.h splay.h object.h heap.h xarray.h config.h std.h
heap.o: heap.c heap.h config.h std.h
ip.o: ip.c ip.h splay.h bi.h gc.h object.h xbarray.h xarray.h config.h std.h
lambda.o: lambda.c ip.h bi.h gc.h object.h xbarray.h xarray.h config.h std.h
lex.o: lex.c xbarray.h lex.h config.h std.h
macro.o: macro.c
number.o: number.c ip.h bi.h gc.h object.h xbarray.h xarray.h config.h std.h
object.o: object.c object.h xbarray.h xarray.h config.h std.h
os.o: os.c ip.h pf.h bi.h gc.h object.h xarray.h xbarray.h config.h std.h
paren.o: paren.c bi.h ip.h splay.h gc.h lex.h object.h xbarray.h pf.h xarray.h xgetopt.h config.h std.h
pf.o: pf.c pf.h xbarray.h config.h std.h
seq.o: seq.c bi.h ip.h gc.h object.h xbarray.h xarray.h config.h std.h
splay.o: splay.c splay.h gc.h object.h config.h std.h
std.o: std.c config.h std.h
xarray.o: xarray.c xarray.h config.h std.h
xbarray.o: xbarray.c xbarray.h config.h std.h
xgetopt.o: xgetopt.c xgetopt.h config.h std.h
