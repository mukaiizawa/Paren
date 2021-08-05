// interpreter.

#define TRAP_NONE 0
#define TRAP_ERROR 1
#define TRAP_INTERRUPT 2 // Ctrl-c
extern int ip_trap_code;

enum error {
  Exception,
    SystemExit,
    Error,
      ArgumentError,
        IndexError,
        ArithmeticError,
      StateError,
        UnicodeError,
        OSError,
        SyntaxError,
  error_nil
};

enum error_msg {
  assert_failed,
  builtin_failed,
  connection_failed,
  division_by_zero,
  expected_array,
  expected_binding_value,
  expected_builtin_operator,
  expected_byte,
  expected_bytes,
  expected_bytes_like,
  expected_collection,
  expected_cons,
  expected_dict,
  expected_function,
  expected_function_macro,
  expected_instance_of_Class_class,
  expected_instance_of_Exception_class,
  expected_integer,
  expected_keyword,
  expected_keyword_parameter_value,
  expected_list,
  expected_loop_context,
  expected_mutable_sequence,
  expected_number,
  expected_operator,
  expected_positive_integer,
  expected_positive_integer_or_sequence,
  expected_sequence,
  expected_string,
  expected_symbol,
  expected_symbol_keyword,
  fgetc_failed,
  fopen_failed,
  fp_failed,
  fputc_failed,
  fread_failed,
  fseek_failed,
  ftell_failed,
  fwrite_failed,
  gethostname_failed,
  index_out_of_range,
  invalid_args,
  invalid_binding_expr,
  invalid_utf8_byte_sequence,
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
  unexpected_mode,
  error_msg_nil
};

extern int ip_throw(enum error e, enum error_msg em);
extern void ip_mark_object(void);
extern void ip_start(object args);
