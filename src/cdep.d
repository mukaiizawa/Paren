ahdrd.o: ahdrd.c ringbuf.h ahdrd.h std.h
env.o: env.c env.h splay.h std.h
lexcer.o: lexcer.c ringbuf.h ahdrd.h lexcer.h paren.h env.h splay.h
paren.o: paren.c writer.h ringbuf.h ahdrd.h lexcer.h reader.h paren.h env.h splay.h std.h
reader.o: reader.c ringbuf.h ahdrd.h lexcer.h reader.h paren.h env.h splay.h
ringbuf.o: ringbuf.c ringbuf.h
splay.o: splay.c splay.h std.h
std.o: std.c std.h
writer.o: writer.c writer.h paren.h env.h splay.h

