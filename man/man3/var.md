# NAME
var - variable binding context.

# SYNOPSIS

    (var (SYM ...)
        EXPR ...)

# DESCRIPTION
Construct a context in which to declare and use symbols for the purpose of reassignment.

# RETURN VALUE
Returns the evaluation result of the last `EXPR`.

# NOTES
Basically, the recommended coding style is not to reassign once bound by `let(3)`, and if `var` macro is used frequently, the coding style should be reviewed for problems.

    (var (SYM1 SYM2 ...)
        EXPR ...)
    <=>
    (let (SYM1 nil SYM2 nil ...)
        EXPR ...)

# EXAMPLES

    ) (var (line)
        (while (<- line (read-line))
          (write-line line)))
    ;; ...

# SEE ALSO
- `let(3)`
