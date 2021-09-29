# NAME
quote - just returns the value.

# SYNOPSIS

    (quote X)

# DESCRIPTION
The special operator `quote` just returns the argument.

# RETURN VALUE
Returns the `X`.

# NOTES
Since quote is used frequently, the reader macro is defined.

    'X <=> (quote X)

# EXAMPLES

    ) 'foo
    foo
    ) (quote foo)
    foo

# SEE ALSO
- special-operator(7)
