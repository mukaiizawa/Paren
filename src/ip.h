// interpreter.

enum Exception {
  Exception,
    SystemExit,
    Error,
      ArgumentError,
      ArithmeticError,
      StateError,
      OSError,
      SyntaxError
};

enum error_msg2 {
  readdir_failed,
  recv_failed,
  send_failed,
  socket_startup_failed,
  stack_over_flow,
  stat_failed,
  too_few_arguments,
  too_many_arguments,
  unbound_symbol,
  undeclared_class,
  undeclared_keyword_param,
  unexpected_keyword_parameter,
  unknown_af_family,
  unknown_socktype,
  error_msg_nil
};

// TODO
extern int ip_throw(enum Exception e, enum error_msg2 em);
extern int ip_sigerr(enum Exception e, char *msg);
extern void ip_mark_object(void);
extern int ip_start(object args);
