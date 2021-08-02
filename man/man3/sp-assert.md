# NAME
assert - provide assertion for debug environment.

# SYNOPSIS

    (assert TEST)

# DESCRIPTION
Evaluates the specified expression and raise error if the results is nil.

# RETURN VALUE
Returns the last evaluation result.

If compiling with the debug option off, returns nil and the expression is not evaluated.

# ERRORS
Error if, `TEST` is nil.

# NOTES
Whether it was compiled in debug mode is held by the global symbol `$debug?(3)`.

# SEE ALSO
- $debug?(3)
- special-operator(7)
