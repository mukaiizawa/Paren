# NAME
assert - abort the program if assertion is false.

# SYNOPSIS

    (assert EXPR)

# DESCRIPTION
The macro `assert` evaluates `EXPR` and aborts if the result is `nil`.

# RETURN VALUE
Returns the evaluation result of the `EXPR`.

# ERRORS
AssertException if, `EXPR` is evaluated to `nil`.

# SEE ALSO
- `raise(3)`
