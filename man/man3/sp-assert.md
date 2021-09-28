# NAME
assert - abort the program if assertion is false.

# SYNOPSIS

    (assert TEST-EXPR)

# DESCRIPTION
The special-operator `assert` evaluates `TEST-EXPR` and aborts if the result is `nil`.

If the symbol `$debug?(3)` is true, the special-operator `assert` is not evaluate `TEST-EXPR`, and hence does nothing at all.

# RETURN VALUE
Undefined.

Do not use the return value as the internal processing will be different depending on whether you compile in debug mode.

# ERRORS
Error if, `TEST-EXPR` is evaluated to nil.

# SEE ALSO
- $debug?(3)
- special-operator(7)
