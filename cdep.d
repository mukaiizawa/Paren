array.o: array.c bi.h ip.h gc.h mem.h object.h splay.h xbarray.h xarray.h config.h std.h
bi.o: bi.c defun.wk defsp.wk ip.h bi.h gc.h object.h splay.h xbarray.h xarray.h config.h std.h
bytes.o: bytes.c bi.h ip.h gc.h mem.h object.h splay.h xbarray.h xarray.h config.h std.h
cons.o: cons.c bi.h ip.h gc.h object.h splay.h xbarray.h xarray.h config.h std.h
gc.o: gc.c gc.h ip.h st.h object.h splay.h heap.h xarray.h config.h std.h
heap.o: heap.c heap.h config.h std.h
ip.o: ip.c ip.h bi.h gc.h object.h splay.h xbarray.h xarray.h config.h std.h
lambda.o: lambda.c ip.h bi.h gc.h object.h splay.h xbarray.h xarray.h config.h std.h
lex.o: lex.c xbarray.h lex.h config.h std.h
number.o: number.c ip.h bi.h gc.h object.h splay.h xbarray.h xarray.h config.h std.h
object.o: object.c object.h splay.h xbarray.h xarray.h config.h std.h
os.o: os.c ip.h pf.h bi.h gc.h object.h splay.h xarray.h xbarray.h xsleep.h config.h std.h
paren.o: paren.c bi.h ip.h gc.h lex.h object.h splay.h xarray.h xbarray.h pf.h xgetopt.h config.h std.h
pf.o: pf.c pf.h xbarray.h config.h std.h
splay.o: splay.c splay.h heap.h config.h std.h
st.o: st.c st.h mem.h object.h splay.h config.h std.h
std.o: std.c config.h std.h
xarray.o: xarray.c xarray.h config.h std.h
xbarray.o: xbarray.c xbarray.h config.h std.h
xgetopt.o: xgetopt.c xgetopt.h config.h std.h
xsleep.o: xsleep.c xsleep.h config.h std.h
