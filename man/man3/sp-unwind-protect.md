# NAME
unwind-protect - guarantee of evaluation.

# SYNOPSIS

    (unwind-protect EXPR CLEANUP-EXPR)

# DESCRIPTION
`unwind-protect` evaluates EXPR and guarantees that CLEANUP-FORMS are executed before `unwind-protect` exits, whether it terminates normally or not.

# RETURN VALUE
undefined.

# NOTES
In general, it is rare to call `unwind-protect` directly, and it is used in a macro like `with-open(3)`.

# EXAMPLES

    ) (function foo () (unwind-protect (return i) (<- i (++ i))))
    foo
    ) (<- i 0)
    0
    ) (foo)
    0
    ) (foo)
    1
    ) i
    2

# SEE ALSO
- special-operator(7)
