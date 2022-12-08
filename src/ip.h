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
  expected_operator,
  expected_positive_integer,
  expected_positive_integer_or_sequence,
  fgetc_failed,
  fopen_failed,
  fp_failed,
  fputc_failed,
  fread_failed,
  fseek_failed,
  ftell_failed,
  fwrite_failed,
  getaddrinfo_failed,
  gethostname_failed,
  incomplete_utf8_byte_sequence,
  index_out_of_range,
  invalid_args,
  invalid_binding_expr,
  numeric_overflow,
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
  unexpected_utf8_leading_byte,
  error_msg_nil
};

// TODO
extern int ip_throw(enum Exception e, enum error_msg2 em);
extern int ip_sigerr(enum Exception e);
extern int ip_sigerr_msg(enum Exception e, char *msg);
extern void ip_mark_object(void);
extern int ip_start(object args);
