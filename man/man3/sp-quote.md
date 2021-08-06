# NAME
quote - just returns the value.

# SYNOPSIS

    (quote VALUE)

# DESCRIPTION
The `quote` special operator just returns VALUE.

# RETURN VALUE
Returns the VALUE.

# NOTES
Since quote is used frequently, the reader macro is defined.

    'VALUE <=> (quote VALUE)

# EXAMPLES

    ) foo
    StateError -- unbound symbol
            at: foo
            at: (repl)
            at: (boot nil)
    ) 'foo
    foo
    ) (quote foo)
    foo

# SEE ALSO
- special-operator(7)
