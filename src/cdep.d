ahdrd.o: ahdrd.c ringbuf.h ahdrd.h std.h
array.o: array.c array.h std.h
env.o: env.c env.h splay.h std.h
gc.o: gc.c gc.h paren.h env.h splay.h stack.h array.h std.h
paren.o: paren.c writer.h ringbuf.h ahdrd.h reader.h gc.h paren.h env.h splay.h stack.h array.h std.h
reader.o: reader.c ringbuf.h ahdrd.h reader.h paren.h env.h splay.h
ringbuf.o: ringbuf.c ringbuf.h
splay.o: splay.c splay.h std.h
stack.o: stack.c stack.h std.h
std.o: std.c std.h
writer.o: writer.c writer.h paren.h env.h splay.h std.h

