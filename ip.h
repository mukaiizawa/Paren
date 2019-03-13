// interpreter

extern int ip_trap_code;
#define TRAP_NONE 0
#define TRAP_ERROR 1
#define TRAP_INTERRUPT 2 /* ctrl-c */
#define TRAP_QUIT 3 /* for WM_CLOSE, etc */

extern void ip_mark_error(char *msg);
extern void ip_mark_too_few_arguments_error(void);
extern void ip_mark_too_many_arguments_error(void);
extern void ip_mark_illegal_arguments_error(int argc, int min, int max);

extern void ip_mark(void);
extern void ip_start(void);
