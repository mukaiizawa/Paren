# NAME
begin0 - evaluate expressions and return the first evaluated result.

# SYNOPSIS

    (begin0 EXPR ...)

# DESCRIPTION
The macro `begin0` evaluates `EXPRs`, in the order in which they are given.

Same as the special-operater `begin(3)` except that it returns the evaluation result of the first `EXPRs`.

# RETURN VALUE
Returns the evaluation result of the first `EXPRs`.

# NOTES
The macro `begin0` is typically used to evaluate one or more expressions with side effects and return a value that must be computed before some or all of the side effects happen.

# EXAMPLES

    ) (<- l '(1 2 3))
    (1 2 3)
    ) (begin0 (pop! l) (pop! l))
    1
    ) l
    (3)

# SEE ALSO
- `begin(3)`
