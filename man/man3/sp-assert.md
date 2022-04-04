# NAME
assert - abort the program if assertion is false.

# SYNOPSIS

    (assert EXPR)

# DESCRIPTION
The special operator `assert` evaluates `EXPR` and aborts if the result is `nil`.

If the symbol `$debug?(3)` is true, `EXPR` is not evaluate.

# RETURN VALUE
Undefined.

Do not use the return value as the internal processing will be different depending on whether you compile in debug mode.

# ERRORS
Error if, `EXPR` is evaluated to nil.

# SEE ALSO
- `$debug?(3)`
- `special-operator(7)`
