// interpreter.

extern int ip_sigerr(enum Exception e, char *msg);
extern void ip_mark_object(void);
extern int ip_start(object args);
