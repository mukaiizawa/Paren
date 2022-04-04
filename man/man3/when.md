# NAME
when - evaluate expressions when condition is not nil.

# SYNOPSIS

    (when TEST
        EXPR ...)

# DESCRIPTION
The macro `when` evaluates `EXPRs`, only if `TEST` is not `nil`.

# RETURN VALUE
Returns the evaluation result of the last `EXPRs`.

# NOTES

    (when TEST EXPR ...) <=> (if TEST (begin EXPR ...))

# EXAMPLES

    ) (<- i 0)
    0
    ) (when true (<- i (++ i)) (<- i (++ i)))
    2
    ) i
    2
    ) (when nil (<- i (++ i)) (<- i (++ i)))
    nil
    ) i
    2

# SEE ALSO
- `if(3)`
- `begin(3)`
