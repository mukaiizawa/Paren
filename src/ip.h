// interpreter.

extern int ip_trap_code;
#define TRAP_NONE 0
#define TRAP_ERROR 1
#define TRAP_INTERRUPT 2 /* ctrl-c */
#define TRAP_QUIT 3 /* for WM_CLOSE, etc */

extern int ip_mark_error(char *msg);
extern void ip_mark_object(void);
extern void ip_start(object args);