# following rules are generated automatically.
array.o: array.c xbarray.h bi.h ip.h gc.h object.h config.h std.h
bi.o: bi.c defun.wk defsp.wk ip.h xbarray.h bi.h gc.h object.h config.h std.h
cons.o: cons.c xbarray.h bi.h ip.h gc.h object.h config.h std.h
dl.o: dl.c xbarray.h bi.h gc.h object.h config.h std.h
gc.o: gc.c gc.h ip.h st.h object.h xarray.h heap.h config.h std.h
heap.o: heap.c heap.h config.h std.h
ip.o: ip.c ip.h xbarray.h bi.h gc.h object.h config.h std.h
lex.o: lex.c xbarray.h lex.h config.h std.h
mem.o: mem.c xbarray.h bi.h ip.h gc.h object.h config.h std.h
number.o: number.c ip.h xbarray.h bi.h gc.h object.h config.h std.h
object.o: object.c gc.h object.h xbarray.h config.h std.h
os.o: os.c ip.h pf.h xbarray.h bi.h gc.h object.h xsleep.h config.h std.h
paren.o: paren.c bi.h ip.h gc.h lex.h object.h xbarray.h pf.h config.h std.h
pf.u.o: pf.u.c xbarray.h pf.h config.h std.h
pf.w.o: pf.w.c xbarray.h pf.h config.h std.h
proc.o: proc.c ip.h xbarray.h bi.h gc.h object.h config.h std.h
sock.o: sock.c ip.h pf.h xbarray.h bi.h gc.h object.h xsleep.h config.h std.h
st.o: st.c st.h object.h config.h std.h
std.o: std.c config.h std.h
xarray.o: xarray.c xarray.h config.h std.h
xbarray.o: xbarray.c xbarray.h config.h std.h
xsleep.u.o: xsleep.u.c xsleep.h config.h std.h
xsleep.w.o: xsleep.w.c xsleep.h config.h std.h
