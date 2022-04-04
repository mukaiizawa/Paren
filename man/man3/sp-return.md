# NAME
return - exits the closest function context.

# SYNOPSIS

    (return VALUE)

# DESCRIPTION
The special operator `return` terminates the current function context and returns `VALUE`.

# RETURN VALUE
Returns the `VALUE`.

# EXAMPLES

    ) (function foo (x) (return :value) x)
    foo
    ) (foo (list 1 2 3))
    :value

# SEE ALSO
- `special-operator(7)`
